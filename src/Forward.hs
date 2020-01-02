module Forward where 

import Language
import Algebra
import Variable

differentiate :: Term -> Term
differentiate = foldTerm
  (Var, fCReal, CInt, CArray, Pair, Fun, fSigmoid, fAdd, fMult, Dot,
   IntAdd, IntMult, New, Length, Lookup, Update, Map, Fold, Case, Apply,
   (fTReal, TInt, TArray, TPair, TFun)) . alphaRename where
  fTReal = TPair TReal TReal
  fCReal n = Pair (CReal n) (CReal 0)
  fAdd t s = Case t "x" "x'" $ Case s "y" "y'"
             (Pair (Var "x" + Var "y")
              (Var "x'" + Var "y'"))
  fMult t s = Case t "x" "x'" $ Case s "y" "y'"
              (Pair (Var "x" * Var "y")
               (Var "x" * Var "y'" + Var "x'" * Var "y"))
  fSigmoid t = Case t "x" "x'" $
               let_ "y" (Sigmoid (Var "x"), TReal)
                (Pair (Var "y")
                 (Var "x'" * Var "y" * (1 - Var "y")),
                  TPair TReal TReal)

