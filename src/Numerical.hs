module Numerical where

import Algebra
import Language
import qualified StackMachine as A2
import PrettyPrint
import StaticCheck

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

delta :: Double
delta = 0.001

evaluate = A2.evaluate M.empty

-- | Finte differences approximates the derivative at a point by looking at its value
--   at that point and at a nearby point.
finiteDifferences :: Term -> [Term] -> Term
finiteDifferences t is = case (evaluate value, evaluate valuePlus) of
                           (CReal v, CReal vp) -> CReal v $* CReal ((vp - v) / delta)
                           (CArray t v, CArray _ vp) -> CArray (t $* t) (V.zipWith (\(CReal x) (CReal y) -> CReal y $* CReal ((x - y) / delta)) vp v)

                           _                   -> error $ "Unexpected term value: " ++ show (value, valuePlus)
  where value = foldl f t is
        f x y = x $$ argProcess (fst_ real real) y

        valuePlus = foldl g t is
        g x y = x $$ argProcess (fun [("z", real $* real)] (case_ (var "z") "x" "y" (var "x" + var "y" * CReal delta), real)) y

dropPair :: Type -> Type
dropPair = foldType (TReal, TInt, TArray, fPair, TFun)
  where fPair TReal TReal = TReal
        fPair t1    t2    = TPair t1 t2

argProcess :: Term -> Term -> Term
argProcess t' t = case typecheck M.empty t of
  Right (TPair TReal TReal) -> t' $$ t
  Right (TPair _     _)     -> case evaluate t of
                                 ~(Pair t1 t2) -> Pair (argProcess t' t1) (argProcess t' t2)
  Right TInt                -> t
  Right (TArray _)          -> case evaluate t of
                                 ~(CArray tau ts) -> CArray (dropPair tau) (V.map (argProcess t') ts)
  _                         -> error "Don't know how to handle this"
