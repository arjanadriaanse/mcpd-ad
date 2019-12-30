{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AbstractMachine where
import Language 
import Algebra
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State.Strict

type Env = M.Map Identifier Term
data MachineState = MachineState Env (State MachineState Term)


local :: State MachineState a -> State MachineState a
local command = do
  m      <- get 
  result <- command
  modify $ const m 
  return result

defaultmachinestate :: MachineState
defaultmachinestate = MachineState M.empty (return (Var "xd"))

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
      Fun <$> t1 <*> t2 <*> return x <*> return (Var "xd")
  fCase e x y e3 = undefined --local $ do
  --    pair <- e 
  --    case pair of 
  --        (Pair e1 e2) -> do
  --          modify (M.insert y e2 . M.insert x e1)
  --          e3
  fApply e1 e2 = local $ do 
        func <- e1
        e    <- e2
        case func of 
            (Fun _ _ x _) -> do
                modify (envInsert x e)
                body <- gets (\(MachineState _ val) -> val)
                body
  fSigmoid   = undefined 
  fDot       = undefined 
  fNew       = undefined 
  fLength       = undefined 
  fLookup       = undefined 
  fUpdate       = undefined 
  fMap       = undefined 
  fFold       = undefined 
  
  fAdd n1 n2 = do 
      r1 <- n1 
      r2 <- n2
      case (r1,r2) of 
          (CReal c1, CReal c2) -> return (CReal (c1 + c2))
  fMult n1 n2 = do 
      r1 <- n1 
      r2 <- n2
      case (r1,r2) of 
          (CReal c1, CReal c2) -> return (CReal (c1 * c2))
  fIntAdd n1 n2 = do 
      r1 <- n1 
      r2 <- n2
      case (r1,r2) of 
          (CInt c1, CInt c2) -> return (CInt (c1 + c2))
  fIntMult n1 n2 = do 
      r1 <- n1 
      r2 <- n2
      case (r1,r2) of 
          (CInt c1, CInt c2) -> return (CInt (c1 * c2))
