module Forward where 

import Language
import Algebra
import Variable

differentiate :: Term -> Term
differentiate = foldTerm
  (Var, fCReal, CInt, CArray, Pair, Fun, fSigmoid, fAdd, fMult, Dot,
   IntAdd, IntMult, New, Length, Lookup, Update, Map, Fold, Case, Apply,
   (fTReal, TInt, TArray, TPair, TFun)) . alphaRename where
  fTReal = real $* real
  fCReal n = CReal n $* 0
  fAdd t s = case_ t "x" "x'" $ case_ s "y" "y'" $
             (var "x" + var "y") $*
             (var "x'" + var "y'")
  fMult t s = case_ t "x" "x'" $ case_ s "y" "y'" $
              (var "x" * var "y") $*
              (var "x" * var "y'" + var "x'" * var "y")
  fSigmoid t = case_ t "x" "x'" $
               let_ "y" (sigmoid (var "x"), real)
               (var "y" $*
                (var "x'" * var "y" * (1 - var "y")),
                 real $* real)

