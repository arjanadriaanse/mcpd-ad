module Language where
import qualified Data.Vector as V 

data Type = TReal
          | TInt
          | TArray Type
          | TPair  Type Type
          | TFun   Type Type
          deriving (Eq, Ord)

type Identifier = String

data Term = Var     Identifier
          | CReal   Double
          | CInt    Int
          | CArray  Type (V.Vector Term)
          | Pair    Term Term
          | Fun     Type Type Identifier Term
          | Sigmoid Term
          | BinOp   BinOp (Maybe Type) Term Term
          | New     Type Term
          | Length  Term
          | Lookup  Term Term
          | Update  Term Term Term
          | Map     Term Term
          | ZipWith Term Term Term 
          | Fold    Term Term Term
          | Case    Term Identifier Identifier Term
          | Apply   Term Term
          | Comp    Term Term 

data BinOp = Add | Mult | Dot



class Pair a where
  ($*) :: a -> a -> a

instance Pair Type where
  ($*) = TPair

instance Pair Term where
  ($*) = Pair

instance Num Term where
  (+)           = BinOp Add Nothing
  (*)           = BinOp Mult Nothing
  abs (CReal n) = CReal (abs n)
  abs _         = error "Not supported"
  signum (CReal n) = CReal (signum n)
  signum _      = error "Not supported"
  fromInteger   = CReal . fromInteger
  negate n      = CReal (-1) * n

instance Fractional Term where
  fromRational = CReal . fromRational
  (/)          = error "Not supported YET" -- TODO

-- | Misc. Sugar
let_ :: Identifier -> (Term, Type) -> (Term, Type) -> Term
let_ x (v, t1) (b, t2) = Fun t1 t2 x b $$ v

-- | Type Sugar
real :: Type 
real = TReal

int :: Type 
int = TInt 

array :: Type -> Type
array = TArray

-- | Term Sugar 
var :: Identifier -> Term 
var = Var 

fun :: [(Identifier, Type)] -> (Term, Type) -> Term 
fun [] (b, _)              = b
fun ((x, t1) : []) (b, t2) = Fun t1 t2 x b
fun ((x, t1) : xs) (b, t2) = Fun t1 (TFun t3 t4) x f
  where 
    f@(Fun t3 t4 _ _) = fun xs (b, t2)

sigmoid :: Term -> Term 
sigmoid = Sigmoid 

dot :: Term -> Term -> Term
dot = BinOp Dot Nothing

($^) :: Term -> Int -> Term
t $^ n = product (replicate n t)

new :: Type -> Term -> Term
new = New 

length_ :: Term -> Term 
length_ = Length 

lookup_ :: Term -> Term -> Term
lookup_ = Lookup 

update :: Term -> Term -> Term -> Term 
update = Update 

map_ :: Term -> Term -> Term 
map_ = Map 

fold :: Term -> Term -> Term -> Term 
fold = Fold

zipwith :: Term -> Term -> Term -> Term 
zipwith = ZipWith

case_ :: Term -> Identifier -> Identifier -> Term -> Term 
case_ = Case 

($$) :: Term -> Term -> Term
($$) = Apply

($.) :: Term -> Term -> Term
($.) = Comp

fst_ :: Type -> Type -> Term
fst_ tau1 tau2 = fun [("x", tau1 $* tau2)] (case_ (var "x") "y" "z" $ var "y", tau1) 

snd_ :: Type -> Type -> Term
snd_ tau1 tau2 = fun [("x", tau1 $* tau2)] (case_ (var "x") "y" "z" $ var "z", tau2)
