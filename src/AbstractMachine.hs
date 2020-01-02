{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AbstractMachine where
import Language 
import Algebra
import qualified Data.Map.Strict as M
import Control.Monad
import PrettyPrint
import Control.Monad.State.Strict
import qualified Data.Vector as V
type Env = M.Map Identifier Term
data MachineState = MachineState Env (State MachineState Term)


local :: State MachineState a -> State MachineState a
local command = do
  m      <- get 
  result <- command
  modify $ const m 
  return result

defaultmachinestate :: MachineState
defaultmachinestate = MachineState M.empty undefined

envLookup :: Identifier -> MachineState -> Maybe Term 
envLookup x (MachineState env _)  = M.lookup x env

envInsert :: Identifier -> Term -> MachineState -> MachineState
envInsert x e (MachineState env t) = MachineState (M.insert x e env) t

evaluate :: Term -> Term 
evaluate t =  evalState (foldTerm (fVar, return . CReal, return . CInt, (\x y -> (liftM2 CArray) x (sequence y)), 
        liftM2 Pair, fFun, fSigmoid, fAdd, fMult, fDot, 
        fIntAdd, fIntMult, fNew, fLength, fLookup, 
        fUpdate, fMap, fFold, fCase, fApply,
        (return TReal, return TInt, liftM TArray, liftM2 TPair, liftM2 TFun)) t) defaultmachinestate where 
  fVar x = do
      v <- gets (envLookup x)
      case v of
        Nothing -> error "Variable not found"
        (Just e) -> return e
  fFun t1 t2 x e  = do
      -- Evaluating a function definition results in nothing
      -- put the function body in the monad
      modify (\(MachineState env _) -> (MachineState env e))
      Fun <$> t1 <*> t2 <*> return x <*> return (error "fFun")
  fCase pairexp x y exp2 = local $ do
      pair <- pairexp
      -- put the variables in the environment and execute exp2
      case pair of 
          (Pair p1 p2) -> do
            modify (envInsert y p2 . envInsert x p1)
            exp2
  fApply e1 e2 = local $ do 
        func <- e1
        arg  <- e2
        case func of 
            (Fun _ _ x _) -> do
                modify (envInsert x arg)
                body <- gets (\(MachineState _ val) -> val)
                body
  fNew t1 n = do 
      len  <- n 
      t    <- t1
      case len of 
          (CInt i) -> case t of 
              TReal -> return $ CArray t (V.replicate i (CReal 0)) -- for now
              _     ->  error (show t)
  fLength e  = do
      array <- e  
      case array of 
          (CArray _ a ) -> return $ CInt (V.length a)
  fLookup e i = do 
      index <- i 
      array <- e 
      case (index, array) of 
          ((CInt i), (CArray _ a)) -> return $ ( a V.! i )
  fUpdate a i v = do 
       array <- a 
       index <- i 
       value <- v
       case (index, array) of
          ((CInt c), (CArray t ar)) -> return $ CArray t ( (V.//) ar [(c, value)] )
  fMap f a = do 
      func  <- f 
      array <- a 
      case (func, array) of 
          ((Fun _ t2 _ _), (CArray _ vec )) -> do 
                newvec <- V.mapM (fApply f) (V.map return vec)
                return $ CArray t2 newvec
  fFold f b a = do 
      func  <- f 
      array <- a 
      start <- b 
      case array of 
          (CArray _ vec ) -> do 
                result <- V.foldM (\x y -> fApply (fApply f (return y)) (return x) ) start vec
                return result
  fSigmoid = operatorUn (\z -> 1 / (1 + exp(-z)))
  fDot     = undefined
  fAdd     = operator (+) -- todo: elementwise
  fMult    = operator (*)
  fIntAdd  = operatorInt (+)
  fIntMult = operatorInt (*)

-- helper functions for binary, unary operators
operatorInt op n1 n2 = do 
    r1 <- n1 
    r2 <- n2
    case (r1,r2) of 
          (CInt c1, CInt c2) -> return (CInt (op c1 c2))

operatorUn op n = do 
    r1 <- n
    case r1 of 
        (CReal c1) -> return (CReal (op c1))

operator op n1 n2 = do 
    r1 <- n1 
    r2 <- n2
    case (r1,r2) of 
          (CReal c1, CReal c2)   -> return (CReal (op c1 c2))