{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AbstractMachine where
import Language 
import Algebra
import qualified Data.Map.Strict as M
import Control.Monad
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
      modify (\(MachineState env _) -> (MachineState env e))
      Fun <$> t1 <*> t2 <*> return x <*> return undefined
  fCase e x y e3 = local $ do
      pair <- e 
      case pair of 
          (Pair e1 e2) -> do
            modify (envInsert y e2 . envInsert x e1)
            e3
  fApply e1 e2 = local $ do 
        func <- e1
        e    <- e2
        case func of 
            (Fun _ _ x _) -> do
                modify (envInsert x e)
                body <- gets (\(MachineState _ val) -> val)
                body
  fNew tau n = do 
      nr   <- n 
      tau2 <- tau
      case nr of 
          (CInt c) -> return $ CArray tau2 (V.replicate c (CReal 0)) -- for now
  fLength t  = do
      array <- t  
      case array of 
          (CArray _ a ) -> return $ CInt (V.length a)
  fLookup t i = do 
      index <- i 
      array <- t 
      case (index, array) of 
          ((CInt c), (CArray _ a)) -> return $ ( a V.! c )
  fUpdate a i v = do 
       array <- a 
       index <- i 
       value <- v
       case (index, array) of
          ((CInt c), (CArray t ar)) -> return $ CArray t ( (V.//) ar [(c, value)] )
  fMap f a = do 
      f2 <- f 
      a2 <- a 
      case (f2, a2) of 
          ((Fun t1 t2 x t), (CArray _ vec )) -> do 
                list <- V.mapM (fApply f) (V.map return vec)
                return $ CArray t2 list
  fFold f b a = do 
      f2 <- f 
      a2 <- a 
      b2 <- b 
      case (f2, a2) of 
          ((Fun t1 t2 x t), (CArray _ vec )) -> do 
                list <- V.foldM (\x y -> fApply (fApply f (return y)) (return x) ) b2 vec
                return list
  fSigmoid = operatorUn (\z -> 1 / (1+ exp(-z)))
  fDot     = undefined
  fAdd     = operator (+) -- elementwise
  fMult    = operator (*)
  fIntAdd  = operatorInt (+)
  fIntMult = operatorInt (*)

operatorInt op n1 n2 = do 
    r1 <- n1 
    r2 <- n2
    case (r1,r2) of 
          (CInt c1, CInt c2)   -> return (CInt (op c1 c2))

operatorUn op n = do 
    r1 <- n
    case r1 of 
        (CReal c1) -> return (CReal (op c1))
operator op n1 n2 = do 
    r1 <- n1 
    r2 <- n2
    case (r1,r2) of 
          (CReal c1, CReal c2)   -> return (CReal (op c1 c2))