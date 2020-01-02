module Main where

import Language
import Forward
import PrettyPrint
import Variable
import AbstractMachine

main :: IO ()
main = putStrLn "Hello, Haskell!"

example0 :: Term 
example0 = Mult (CReal 2) (Var "x")

example :: Term
example = Apply (Fun TReal TReal "y" (Add (Var "y") (Var "y"))) (CReal 10)

exampleNest :: Term 
exampleNest = Apply (Fun TReal TReal "x" (Apply f (CReal 20))) (CReal 8000)
    where 
        f = Fun TReal TReal "y" (Add (Var "x") (Var "y")) 

exampleFst :: Term
exampleFst = Apply (Fun (TPair TReal TReal) (TReal) "x" (Case (Var "x") "id1" "id2" (Var "id1") )) (Pair (CReal 1) (CReal 20))

example2 :: Term
example2 = Fun TReal TReal "x" ((Fun TReal TReal "x" (Var "x" `Mult` Var "x")) `Apply` (Var "x"))

exampleMap :: Term 
exampleMap = Map (Fun TReal TReal "x" ( Add (Var "x") (CReal 20))) (New TReal (CInt 10))

newIndexList :: Term 
newIndexList = Update (New TReal (CInt 10)) (CInt 1) (5 + 9)


examplef :: Term
examplef = Fold f (CReal 0) exampleMap where 
    f = Fun TReal TReal "b" ((Fun TReal TReal "x" (Var "x" `Add` Var "b"))) 

    -- def hallo(typle, yple, yple) 


testNested = f $$ CReal 10 $$ 5 $$ 3
    where 
        f = fun [("x", TReal),("y", TReal), ("z", TReal)] ((Var "x") * ((Var "y") + (Var "z")), TReal) 
