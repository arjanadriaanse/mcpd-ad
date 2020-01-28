{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Annotate where
import Language hiding (Env) 
import Algebra
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

type Env = M.Map Identifier Type

local :: State Env a -> State Env a
local command = do
  e <- get
  result <- command
  put e
  return result

annotate :: Term -> Term
annotate tm = snd $ evalState (foldTerm (fVar, 
        return . (,) real . CReal, 
        return . (,) int . CInt, 
        fCArray, fPair, fFun, fSigmoid, fBinOp, fNew, fLength, fLookup, 
        fUpdate, fMap, fZipWith, fFold, fCase, fApply, fComp,
        idTypeAlgebra) tm) M.empty where 
  fVar x = do
    t <- gets (M.! x)
    return (t, var x)
  fCArray t a = (,) (array t) . CArray t . fmap snd <$> sequence a
  fPair f s = do
      (t1, e1) <- f
      (t2, e2) <- s
      return (t1 $* t2, e1 $* e2) 
  fFun t1 t2 x e  =  local $ modify (M.insert x t1) >>
      (,) (TFun t1 t2) . Fun t1 t2 x . snd <$> e
  fCase e x y b = local $ do
    (~(TPair t1 t2), ep) <- e
    modify (M.insert y t2 . M.insert x t1)
    (t, eb) <- b
    return (t, Case ep x y eb)
  fApply f e = do
    (~(TFun _ t), ef) <- f
    (_, ee) <- e
    return (t, Apply ef ee)
  fComp f1 f2 = do
    (~(TFun _ t2), ef1) <- f1
    (~(TFun t1 _), ef2) <- f2
    return (TFun t1 t2, Comp ef1 ef2)
  fNew t1 n = (,) (array t1) . New t1 . snd <$> n
  fLength e = (,) int . Length . snd <$> e
  fLookup e i = do
    (~(TArray t), ee) <- e
    (_, ei) <- i
    return (t, Lookup ee ei)
  fUpdate e i v = do
    (t, ee) <- e
    (_, ei) <- i
    (_, ev) <- v
    return (t, Update ee ei ev)
  fMap f e = do
    (~(TFun _ t), ef) <- f
    (_, ee) <- e
    return (TArray t, Map ef ee)
  fZipWith f e1 e2 = do
    (~(TFun _ (TFun _ t)), ef) <- f
    (_, ee1) <- e1
    (_, ee2) <- e2
    return (TArray t, ZipWith ef ee1 ee2)
  fFold f v e = do
    (_, ef) <- f
    (t, ev) <- v
    (_, ee) <- e
    return (t, Fold ef ev ee)
  fSigmoid t = (,) real . Sigmoid . snd <$> t
  fBinOp Dot _ e1 e2 = do
    (_, ee1) <- e1
    (_, ee2) <- e2
    return (real, BinOp Dot (Just $ array real) ee1 ee2)
  fBinOp o _ e1 e2 = do
    (t, ee1) <- e1
    (_, ee2) <- e2
    return (t, BinOp o (Just t) ee1 ee2)
