module Main where

import Language
import Forward
import PrettyPrint
import Variable

main :: IO ()
main = putStrLn "Hello, Haskell!"


example :: Term
example = Fun TReal TReal "x" (Mult (Var "x") (Var "x"))

exampleFst :: Term
exampleFst = Fun (TPair TReal TReal) (TReal) "x" (Case (Var "x") "id1" "id2" (Var "id1") )

