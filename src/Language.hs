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

instance Fractional Term where
  fromRational = CReal . fromRational
  (/) = undefined -- TODO

($$) :: Term -> Term -> Term
($$) = Apply

fun :: [(Identifier, Type)]  -> Term -> Type -> Term 
fun [] b _              = b 
fun ((x, t1) : []) b t2 = Fun t1 t2 x b
fun ((x, t1) : xs) b t2 = Fun (TFun t1 t3) t2 x f
  where 
    f@(Fun t3 _ _ _) = fun xs b t2

let_ :: Identifier -> Term -> Type -> Term -> Type -> Term
let_ x v t1 b t2 = Fun t1 t2 x b $$ v
