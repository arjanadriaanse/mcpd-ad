module Test where

import Language
import AbstractMachine

import qualified Data.Vector as V

delta :: Double
delta = 0.001

-- | Finte differences approximates the derivative at a point by looking at its value
--   at that point and at a nearby point.
finiteDifferences :: Term -> [Term] -> Term
finiteDifferences t is = case (evaluate value, evaluate valuePlus) of
                           (CReal v, CReal vp) -> CReal v $* CReal ((vp - v) / delta)
                           (CArray t v, CArray _ vp) -> CArray (t $* t) (V.zipWith (\(CReal x) (CReal y) -> CReal y $* CReal ((x - y) / delta)) vp v)

                           _                   -> error $ "Unexpected term value: " ++ show (value, valuePlus)
  where value = foldl f t is
        f x y = x $$ case evaluate y of
          Pair _ _ -> fst_ real real $$ y
          CArray _ _ -> map_ (fst_ real real) y

        valuePlus = foldl g t is
        g x y = x $$ case evaluate y of
          Pair _ _ -> case_ y "x" "y" (var "x" + var "y" * CReal delta)
          CArray _ _ -> map_ (fun [("p", real $* real)] (case_ (var "p") "x" "y" (var "x" + var "y" * CReal delta), real)) y
