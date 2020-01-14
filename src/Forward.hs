module Forward where 

import Language
import Algebra
import Variable

differentiate :: Term -> Term
differentiate = foldTerm
  (Var, fCReal, CInt, CArray, Pair, Fun, fSigmoid, fBinOp,
    New, Length, Lookup, Update, Map, ZipWith, Fold, Case, Apply, Comp,
   (fTReal, TInt, TArray, TPair, TFun)) . alphaRename where
  fTReal                            = real $* real
  fCReal n                          = CReal n $* 0
  fBinOp Add  (Just TReal) t s      = case_ t "x" "x'" $ case_ s "y" "y'" $
    (var "x" + var "y") $* (var "x'" + var "y'")
  fBinOp Mult (Just TReal) t s      = case_ t "x" "x'" $ case_ s "y" "y'" $
    (var "x" * var "y") $* (var "x" * var "y'" + var "x'" * var "y")
  fBinOp Add  (Just TInt) t s       = BinOp Add (Just TInt) t s
  fBinOp Mult (Just TInt) t s       = BinOp Add (Just TInt) t s
  fBinOp Add  (Just (TArray a)) t s = undefined -- TODO
  fBinOp Mult (Just (TArray a)) t s = undefined -- TODO
  fBinOp Dot  (Just (TArray a)) t s = undefined -- TODO
  fBinOp _ _ _ _                    = error "operation not supported"
  fSigmoid t                        = case_ t "x" "x'" $ let_ "y" (sigmoid (var "x"), real)
    (var "y" $* (var "x'" * var "y" * (1 - var "y")), real $* real)
