{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Variable where

import Language
import Algebra

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe

class Counter a where
  add :: State a Int

instance Counter Int where
  add = state $ \i -> (i, i + 1)

instance Counter AlphaRenameState where
  add = state (\(i, m) -> (i, (i + 1, m)))

-- | Generates a fresh variable name
fresh :: Counter a => Identifier -> State a Identifier
fresh x = (x ++) . show <$> add

declare :: Identifier -> State AlphaRenameState Identifier
declare x = do
  y <- fresh "a"
  modify (\(i, m) -> (i, M.insert x y m))
  return y

rename :: Identifier -> State AlphaRenameState Identifier
rename x = fromMaybe (error $ "undeclared variable " ++ x) <$> gets (M.lookup x . snd)

local :: State AlphaRenameState a -> State AlphaRenameState a
local command = do
  m <- gets snd
  result <- command
  modify $ \(i, _) -> (i, m)
  return result

type AlphaRenameState = (Int, M.Map Identifier Identifier)

-- | Make all variable names unique
alphaRename :: Term -> Term
alphaRename t = evalState (foldTerm (fVar, return . CReal, return . CInt, \x y -> (liftM2 CArray) x (sequence y), liftM2 Pair, 
                fFun, liftM Sigmoid, fBinOp, liftM2 New, 
                liftM Length, liftM2 Lookup, liftM3 Update, liftM2 Map,  liftM3 Fold, fCase, liftM2 Apply,
        (return TReal, return TInt, liftM TArray, liftM2 TPair, liftM2 TFun)) t) (0, M.empty) where
  fVar x           = Var <$> rename x
  fFun t1 t2 x e   = local $ Fun <$> t1 <*> t2 <*> declare x <*> e
  fCase e1 x y e2  = local $ Case <$> e1 <*> declare x <*> declare y <*> e2
  fBinOp o (Just t) e1 e2 = t >>= (\x -> BinOp <$> return o <*> return (Just x) <*> e1 <*> e2) -- TODO make Maybe-invariant
  fBinOp o (Nothing) e1 e2 = BinOp <$> return o <*> return Nothing <*> e1 <*> e2 -- TODO make Maybe-invariant
