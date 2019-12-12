module Language where

data Type = TInt
          | TReal
          | TArray Type
          | TFun Type Type

type Identifier = String

data Term = Var Identifier
          | CReal Double
          | CInt Int
          | New Type Term
          | Lookup Term Term
          | Update Term Term Term
          | Length Term
          | Map Term Term
          | Fold Term Term Term
          | Fun Type Type Identifier Term
          | FunApp Term Term
          -- Operators
          | Neg Term
          | Sigmoid Term
          | Add Term Term
          | Mult Term Term
          | InnerProd Term Term
