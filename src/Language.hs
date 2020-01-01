module Language where
import qualified Data.Vector as V 

data Type = TReal
          | TInt
          | TArray Type
          | TPair  Type Type
          | TFun   Type Type
          

type Identifier = String

data Term = Var     Identifier
          | CReal   Double
          | CInt    Int
          | CArray  Type (V.Vector Term)
          | Pair    Term Term
          | Fun     Type Type Identifier Term
          | Sigmoid Term
          | Add     Term Term
          | Mult    Term Term
          | Dot     Term Term
          | IntAdd  Term Term
          | IntMult Term Term
          | New     Type Term
          | Length  Term
          | Lookup  Term Term
          | Update  Term Term Term
          | Map     Term Term
          | Fold    Term Term Term
          | Case    Term Identifier Identifier Term
          | Apply   Term Term

instance Num Term where
  (+) = Add
  (*) = Mult
  abs (CInt n) = CInt (abs n)
  abs (CReal n) = CReal (abs n)
  abs _ = undefined
  signum (CInt n) = CInt (signum n)
  signum (CReal n) = CReal (signum n)
  signum _ = undefined
  fromInteger = CInt . fromInteger
  negate n = CInt (-1) * n

($$) :: Term -> Term -> Term
($$) = Apply


