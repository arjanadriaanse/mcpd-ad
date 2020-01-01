module Backward where 

import Language
import Algebra

differentiate :: Int -> Term -> Term
differentiate k = foldTerm
  (Var, fReal, CInt, Pair, Case, New, Lookup, Update, Length,
   Map, Fold, Fun, FunApp, fSigmoid, fAdd, fMult, Dot,
   (TInt, fTReal, TPair, TArray, TFun)) where
  fTReal = TPair TReal (TFun TReal (TArray TReal))
  fReal n = Pair (CReal n) (Fun TReal (TArray TReal) "z" (New TReal (CInt k)))
  fAdd t s = Case t "x" "x'" (Case s "y" "y'"
                              (Pair (Var "x" `Add` Var "y")
                               (Var "x'" `Add` Var "y'")))
  fMult t s = Case t "x" "x'" (Case s "y" "y'"
                               (Pair (Var "x" `Add` Var "y")
                                ((Var "x" `Mult` Var "y'") `Add`
                                 (Var "x'" `Mult` Var "y"))))
  fSigmoid t = Case t "x" "x'"
               (Fun TReal (TPair TReal TReal) "y"
                (Pair (Var "y")
                 (Var "x'" `Mult` Var "y" `Mult`
                  (CReal 1 `Add` (CReal (-1) `Mult` Var "y"))))
                `FunApp` Sigmoid (Var "x"))
