module Main where

import Language
import Forward
import PrettyPrint
--import StaticCheck
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
        f = fun [("y", real)] (var "y" + var "y", real) 

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

realpair = real $* real
exampleSnd :: Term
exampleSnd = fun [("pair", TPair realpair realpair)] ( case_ (var "pair") "id1" "id2" (var "id2") , realpair)

examplePoly :: Term
examplePoly = fun [("x", real)] ( (3 * (var "x" * var "x" * var "x")) + var "x" * 9 ,real)

exampleLogReg :: Term
exampleLogReg = fun [("x", array real), ("w", array real), ("b", real)] (sigmoid (var "x" `dot` var "w" + var "b"), real)

-- | How to do function composition without polymorphism?

exampleSecondDerivative :: Term 
exampleSecondDerivative =  (fun [("x", realpair)] (exampleSnd $$ ((differentiate examplePoly) $$ (var "x")), real)) -- ?? How to do it

exampleFromPaper :: Term 
exampleFromPaper = fun [("x", real), ("y", real)] (snd_ real real $$ inner, real) where 
    inner    = (differentiate innerfun) $$ ((var "x") $* 1)
    innerfun = (var "x") * ( snd_ real real $$ dfy )
    dfy      = (differentiate (var "x" + var "y")) $$ ((var "y") $* 1)


exampleZip :: Term 
exampleZip = zipwith f exampleMap exampleMap where 
    f = fun [("x", real), ("y", real)] ( var "x" - (2 * var "y") , real )

exampleComp :: Term 
exampleComp = comp where 
    comp = f1 $. f2
f1 = fun [("x", real)] (var "x" + 9, real )
f2 = fun [("y", real)] (var "y" * 2, real ) 

exampleCompFromMain = comptesting f1 f2 

comptesting :: Term -> Term -> Term
comptesting func1 func2 = case (func1, func2) of 
          ((Fun tf11 tf12 id1 body1), (Fun tf21 tf22 id2 body2)) -> result where 
              result = Fun tf11 tf22 id1 body 
              body   = let_ id2 (body1, tf12) (body2, tf22)
