module Main where

import Language
import Forward
import PrettyPrint
import Variable
import AbstractMachine

main :: IO ()
main = putStrLn "Hello, Haskell!"

example0 :: Term 
example0 = 2 * var "x"

example :: Term
example = test $$ (10 $* 1)

test :: Term
test = differentiate $ fun [("y", real)] (var "y" * Var "y" + Var "y", real)

exampleNest :: Term 
exampleNest = fun [("y", real)] (f $$ 20, real) $$ 8000
    where 
        f = fun [("y", real)] (var "x" + var "y", real) 

exampleFst :: Term
exampleFst = Apply (Fun (TPair TReal TReal) (TReal) "x" (Case (Var "x") "id1" "id2" (Var "id1") )) (Pair (CReal 1) (CReal 20))

example2 :: Term
example2 = Fun TReal TReal "x" ((Fun TReal TReal "x" (Var "x" `Mult` Var "x")) `Apply` (Var "x"))

exampleMap :: Term 
exampleMap = Map (Fun TReal TReal "x" ( Add (Var "x") (CReal 20))) (New TReal (CInt 10))

exampleDot :: Term 
exampleDot = Dot exampleMap exampleMap


newIndexList :: Term 
newIndexList = update (new real (CInt 10)) (CInt 1) (5.1 + 9)

examplef :: Term
examplef = fold f 0 exampleMap where 
    f = fun [("b", real), ("x", real)] (var "x" + var "b", real) 


testNested = f $$ CReal 10 $$ 5 $$ 3
    where 
        f = fun [("x", TReal),("y", TReal), ("z", TReal)] ((Var "x") * ((Var "y") + (Var "z")), TReal) 
