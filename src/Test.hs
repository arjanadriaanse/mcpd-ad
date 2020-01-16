module Test where

import Language
import AbstractMachine

-- | Finte differences approximates the derivative at a point by looking at its value
--   at that point and at a nearby point.
finiteDifferences :: Term -> [Term] -> (Double, Double)
finiteDifferences t is = case (evaluate value, evaluate valuePlus) of
                           (CReal v, CReal vp) -> (v, (vp - v) / 0.001)
                           _                   -> error $ "Unexpected term value: " ++ show (value, valuePlus)
  where value = foldl (\x y -> x $$ (fst_ real real $$ y)) t is
        valuePlus = foldl (\x y -> x $$ case_ y "x" "y" (var "x" + var "y" * 0.001)) t is
