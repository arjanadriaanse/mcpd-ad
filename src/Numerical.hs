module Numerical where

import Language
import qualified StackMachine as A2
import PrettyPrint

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
        dropPair t = case t of
          TPair TReal TReal -> TReal
          TArray t'         -> TArray (dropPair t')
          _                 -> error $ "Error in dropPair: " ++ show t
        addMap t x = case t of
          TPair TReal TReal -> x
          TArray t'         -> (fun [("f", TFun t' (dropPair t')), ("x", array t')] (map_ (var "f") (var "x"), array (dropPair t'))) $$ addMap t' x
          _                 -> error $ "Error in addMap: " ++ show t
        f x y = x $$ case evaluate y of
          Pair _ _ -> fst_ real real $$ y
          CArray t _ -> (addMap t (fun [("x", array (real $* real))] (map_ (fst_ real real) (var "x"), array real))) $$ y

        valuePlus = foldl g t is
        g x y = x $$ case evaluate y of
          Pair _ _ -> case_ y "x" "y" (var "x" + var "y" * CReal delta)
          CArray t _ -> (addMap t (fun [("x", array (real $* real))] (map_ (fun [("p", real $* real)] (case_ (var "p") "x" "y" (var "x" + var "y" * CReal delta), real)) (var "x"), array real))) $$ y
