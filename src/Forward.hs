module Forward where 

import Language
import Algebra

differentiate :: Term -> Term
differentiate = foldTerm
  (Var, fReal, CInt, Pair, Case, Fun, Apply, New, Length, Lookup,
   Update, Map, Fold, fSigmoid, fAdd, fMult, Dot, IntAdd, IntMult,
   (fTReal, TInt, TPair, TArray, TFun)) where
  fTReal = TPair TReal TReal
  fReal n = Pair (CReal n) (CReal 0)
  fAdd t s = Case t "x" "x'" (Case s "y" "y'"
                              (Pair (Var "x" `Add` Var "y")
                               (Var "x'" `Add` Var "y'")))
  fMult t s = Case t "x" "x'" (Case s "y" "y'"
                               (Pair (Var "x" `Mult` Var "y")
                                ((Var "x" `Mult` Var "y'") `Add`
                                 (Var "x'" `Mult` Var "y"))))
  fSigmoid t = Case t "x" "x'"
               (Fun TReal (TPair TReal TReal) "y"
                (Pair (Var "y")
                 (Var "x'" `Mult` Var "y" `Mult`
                  (CReal 1 `Add` (CReal (-1) `Mult` Var "y"))))
                `Apply` Sigmoid (Var "x"))
