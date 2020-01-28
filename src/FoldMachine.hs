{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FoldMachine where
import Language 
import Algebra
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Vector as V
import Data.Function (on)
import PrettyPrint

data MachineState = MachineState Env (State MachineState Term)


local :: State MachineState a -> State MachineState a
local command = do
  MachineState e _ <- get
  result <- command
  modify $ \(MachineState _ f) -> MachineState e f
  return result

defaultmachinestate :: MachineState
defaultmachinestate = MachineState M.empty undefined

envLookup :: Identifier -> MachineState -> Maybe Term 
envLookup x (MachineState env _)  = M.lookup x env

envInsert :: Identifier -> Term -> MachineState -> MachineState
envInsert x e (MachineState env t) = MachineState (M.insert x e env) t

evaluate :: Term -> Term 
evaluate t =  evalState (foldTerm (fVar, return . CReal, return . CInt, (\x y -> liftM (CArray x) (sequence y)), 
        liftM2 Pair, fFun, fSigmoid, fBinOp, fNew, fLength, fLookup, 
        fUpdate, fMap, fZipWith, fFold, fCase, fApply, fComp,
        idTypeAlgebra) t) defaultmachinestate where 
  fVar x = do
      v <- gets (envLookup x)
      case v of
        Nothing -> error ("Variable " ++ x ++  " not found")
        (Just e) -> return e
  fFun t1 t2 x e  = local $ do
      -- Evaluating a function definition results in nothing
      -- put the function body in the monad
      modify (\(MachineState env _) -> (MachineState env e))
      return $ Fun t1 t2 x (error ("Function cannot be evaluated"))
  fCase pairexp x y exp2 = local $ do
      pair <- pairexp
      -- put the variables in the environment and execute exp2
      case pair of 
          (Pair p1 p2) -> do
            modify (envInsert y p2 . envInsert x p1)
            exp2
          _ -> error ("No pair provided")
  fApply e1 e2 = do 
        arg  <- e2
        func <- e1
        case func of 
            (Fun _ _ x _) -> do
                modify (envInsert x arg)
                body <- gets (\(MachineState _ val) -> val)
                body
            t -> error "application on non-function term"
  fNew t1 n = do 
      len  <- n 
      case len of 
          (CInt i) -> case t1 of 
              TReal -> return $ CArray t1 (V.replicate i (CReal 0))
              TInt  -> return $ CArray t1 (V.replicate i (CInt 0))
              TPair TReal TReal -> return $ CArray t1 (V.replicate i (0 $* 0 ))
              _     -> error ("Array type " ++ show t1 ++ " not supported")
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
  fZipWith f t1 t2 = do
      func <- f 
      array1 <- t1
      array2 <- t2 
      case (func, array1, array2) of 
          ((Fun _ (TFun _ het_type) _ _), (CArray  _ vec1 ), (CArray _ vec2 )) -> do 
              newvec <- V.zipWithM (\x y -> fApply (fApply f (return x)) (return y) )  vec1 vec2 
              return $ CArray het_type newvec
  fFold f b a = do 
      func  <- f 
      array <- a 
      start <- b 
      case array of 
          (CArray _ vec ) -> do 
                result <- V.foldM (\x y -> fApply (fApply f (return y)) (return x) ) start vec
                return result          
  -- (f1 $. f2) x    =  f1 ( f2 x )
  fComp f2 f1   = do 
      ef1 <- f1
      (MachineState _ b) <- get
      ef2 <- f2
      case (ef1, ef2) of 
          ((Fun t1 _ id _), (Fun _ t2 _ _)) -> fFun t1 t2 id (fApply f2 b)
              
  fSigmoid      = operatorUn (\z -> 1 / (1 + exp(-z)))
  fBinOp Dot t  = dotProduct (fBinOp Mult t ) (fBinOp Add t)
  fBinOp Add t  = operatorBin (+) t -- todo: elementwise
  fBinOp Mult t = operatorBin (*) t



operatorUn op n = do 
    r1 <- n
    case r1 of 
        (CReal c1) -> return (CReal (op c1))
        (CInt c1)  -> return (CReal (op (fromIntegral c1)))
        (CArray t c1) -> return (CArray t (V.map (\(CReal x) -> CReal (op x)) c1))


-- helper function for elementwise adding
unpackBinOp op = (\(CReal x) -> \(CReal y) -> CReal (op x y ))

operatorBin op t n1 n2 = do 
    r1 <- n1 
    r2 <- n2
    case (r1,r2) of 
        (CReal c1, CReal c2)  -> return (CReal (op c1 c2))
        (CArray t c1, CArray _ c2) -> return (CArray t (V.zipWith (unpackBinOp op) c1 c2))
        (CArray t c1, CReal c2) -> return (CArray t (V.map (\(CReal x) -> CReal (op x c2)) c1))
        (CReal c1, CArray t c2) -> return (CArray t (V.map (\(CReal x) -> CReal (op c1 x)) c2))

dotProduct fMult fAdd n1 n2 = do 
    v1 <- n1 
    v2 <- n2 
    case (v1, v2) of 
        ((CArray _ vec1), CArray _ vec2) -> do
             (V.zipWithM (fMult `on` return) vec1 vec2 ) >>= V.foldM (fAdd `on` return) 0

