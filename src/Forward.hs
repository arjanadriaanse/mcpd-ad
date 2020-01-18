module Forward where 

import Language
import Algebra
import Variable

differentiate :: Term -> Term
differentiate = foldTerm
  (Var, fCReal, CInt, CArray, Pair, Fun, fSigmoid, fBinOp,
    New, Length, Lookup, Update, Map, ZipWith, Fold, Case, Apply, Comp,
   (fTReal, TInt, TArray, TPair, TFun, UnknownType)) . alphaRename where
  fTReal                            = real $* real
  fCReal n                          = CReal n $* 0
  fBinOp Add  (Just (TPair TReal TReal)) t s = case_ t "x" "x'" $ case_ s "y" "y'" $
    (var "x" + var "y") $* (var "x'" + var "y'")
  fBinOp Mult (Just (TPair TReal TReal)) t s = case_ t "x" "x'" $ case_ s "y" "y'" $
    (var "x" * var "y") $* (var "x" * var "y'" + var "x'" * var "y")
  fBinOp Add  (Just TInt) t s       = BinOp Add (Just TInt) t s
  fBinOp Mult (Just TInt) t s       = BinOp Add (Just TInt) t s
  fBinOp Add  (Just (TArray (TPair TReal TReal))) t s = ZipWith addArray' t s 
  fBinOp Mult (Just (TArray (TPair TReal TReal))) t s = ZipWith multArray' t s 
  fBinOp Dot  (Just (TArray (TPair TReal TReal))) t s = fold addTuple (0 $* 0 ) (ZipWith multArray' t s )
  fBinOp _ _ _ _                    = error "operation not supported"
  fSigmoid t                        = case_ t "x" "x'" $ let_ "y" (sigmoid (var "x"), real)
    (var "y" $* (var "x'" * var "y" * (1 - var "y")), real $* real)


addArray' :: Term 
addArray' = fun [("t", real $* real), ("s", real $* real)] (
    case_ (var "t") "x" "x'" $ case_ (var "s") "y" "y'" $  (var "x" + var "y") $* (var "x'" + var "y'")
    , real $* real )

multArray' :: Term 
multArray' = fun [("t", real $* real), ("s", real $* real)] (case_ (var "t") "x" "x'" $ case_ (var "s") "y" "y'" $
    (var "x" * var "y") $* (var "x" * var "y'" + var "x'" * var "y"), real $* real )

addTuple :: Term
addTuple = fun [("x",real $* real), ("y", real $* real )] (case_ (var "x") "b0" "b1" $ 
        case_ (var "y") "c0" "c1" ((var "b0" + var "c0") $* (var "b1" + var "c1"))  , real $* real)