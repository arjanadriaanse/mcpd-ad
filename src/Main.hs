module Main where

import Language
import Forward
import PrettyPrint
import Variable
import AbstractMachine

main :: IO ()
main = putStrLn "Hello, Haskell!"

example :: Term
example = df $$ (10 $* 1)
    where
        df = differentiate $ fun [("y", real)] (var "y" * var "y" + var "y", real)

exampleNest :: Term 
exampleNest = fun [("y", real)] (f $$ 20, real) $$ 8000
    where 
        f = fun [("y", real)] (var "x" + var "y", real) 

exampleFst :: Term
exampleFst = fun [("x", TPair real real)] ( case_ (var "x") "id1" "id2" (var "id1") , real) $$ (Pair 10 20) 

exampleMap :: Term 
exampleMap = map_ (fun [("x", real)] (var "x" + 20, real)) (new real (CInt 10))

exampleDot :: Term 
exampleDot = dot exampleMap exampleMap

newIndexList :: Term 
newIndexList = update (new real (CInt 10)) (CInt 1) (5.1 + 9)

examplef :: Term
examplef = fold f 0 exampleMap where 
    f = fun [("b", real), ("x", real)] (var "x" + var "b", real) 

exampleNested :: Term
exampleNested = f $$ 10 $$ 5 $$ 3
    where 
        f = fun [("x", real),("y", real), ("z", real)] (var "x" * (var "y" + var "z"), real) 

-- 9x + 3x^3 
-- 9  + 9x^2 
-- 18x      

realpair = TPair real real 
exampleSnd :: Term
exampleSnd = fun [("pair", TPair realpair realpair)] ( case_ (var "pair") "id1" "id2" (var "id2") , realpair)

examplePoly :: Term
examplePoly = fun [("x", real)] ( (3 * (var "x" * var "x" * var "x")) + var "x" * 9 ,real)

exampleSecondDerivative :: Term 
exampleSecondDerivative = examplePoly -- ?? How to do it
