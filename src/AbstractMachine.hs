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

local :: State Env a -> State Env a
local command = do
  m      <- get 
  result <- command
  modify $ const m 
  return result


evaluate :: Term -> Term 
evaluate t =  evalState (foldTerm (fVar, return . CReal, return . CInt, (\x y -> (liftM2 CArray) x (sequence y)), 
        liftM2 Pair, fFun, fSigmoid, fAdd, fMult, fDot, 
        fIntAdd, fIntMult, fNew, fLength, fLookup, 
        fUpdate, fMap, fFold, fCase, fApply,
        (return TReal, return TInt, liftM TArray, liftM2 TPair, liftM2 TFun)) t) (M.empty ) where 
  fVar x = do
      v <- gets (M.lookup x)
      case v of
        Nothing -> return $ Var x
        (Just e) -> return e
  fFun t1 t2 x e  = 
      Fun <$> t1 <*> t2 <*> return x <*> e
  fCase e x y e3 = local $ do
      pair <- e 
      case pair of 
          (Pair e1 e2) -> do
            modify (M.insert y e2 . M.insert x e1)
            e3
  fApply e1 e2 = local $ do 
        func <- e1
        e    <- e2
        case func of 
            (Fun t1 t2 x _) -> do
                modify (M.insert x e) 
                result <- e1
                case result of 
                    (Fun _ _ _ r) -> return r
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
          (Var x, v) -> return (Mult (Var x) v)
          (v, Var x) -> return (Mult v (Var x))
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
