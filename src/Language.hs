module Language where

data Type = TReal
          | TInt
          | TPair  Type Type
          | TArray Type
          | TFun   Type Type
          

type Identifier = String

data Term = Var Identifier
          | CReal   Double
          | CInt    Int
          | Pair    Term Term
          | Case    Term Identifier Identifier Term
          | Fun     Type Type Identifier Term
          | Apply   Term Term
          | New     Type Term
          | Length  Term
          | Lookup  Term Term
          | Update  Term Term Term
          | Map     Term Term
          | Fold    Term Term Term
          -- Operators
          | Sigmoid Term
          | Add     Term Term
          | Mult    Term Term
          | Dot     Term Term
          | IntAdd  Term Term
          | IntMult Term Term
