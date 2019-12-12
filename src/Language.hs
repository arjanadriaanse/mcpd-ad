module Language where

data Type = TInt
          | TReal
          | TPair Type Type
          | TArray Type
          | TFun Type Type
          

type Identifier = String

data Term = Var Identifier
          | CReal  Double
          | CInt   Int
          | Pair    Term Term
          | Case    Term Identifier Identifier Term
          | New    Type Term
          | Lookup Term Term
          | Update Term Term Term
          | Length Term
          | Map    Term Term
          | Fold   Term Term Term
          | Fun    Type Type Identifier Term
          | FunApp Term Term
          -- Operators
          | Sigmoid Term
          | Add     Term Term
          | Mult    Term Term
          | Dot     Term Term
          
