module Forward where 

import Language
import Algebra

differentiate :: Term -> Term
differentiate = foldTerm (fVar, fReal, fInt, fPair, fCase, fNew, fLook, fUp, fLen, fMap, 
        fFold, fFun, fApp, fNeg, fSig, fAdd, fMult, fDot,
        (fTInt, fTReal, fTPair, fTArray, fTFun)) where
            fVar  = Var
            fReal n = Pair (CReal n) (CReal 0)
            fInt  = CInt

            -- Pair, case
            fPair = Pair
            fCase = Case
            
            -- The rest
            fNew = New
            fLook = Lookup
            fUp = Update
            fLen = Length
            fMap = Map
            fFold = Fold
            fFun = Fun
            fApp = FunApp
            
            -- Operators
            fNeg = Neg
            fSig t = Case t "x" "x'"
              (Fun TReal (TPair TReal TReal) "y"
               (Pair (Var "y") (Var "x'" `Mult` Var "y" `Mult`
                                (CReal 1 `Add` Neg (Var "y"))))
               `FunApp` Sigmoid (Var "x"))
            fAdd t s = Case t "x" "x'" (Case s "y" "y'"
                                        (Pair (Var "x" `Add` Var "y")
                                         (Var "x'" `Add` Var "y'")))
            fMult t s = Case t "x" "x'" (Case s "y" "y'"
                                         (Pair (Var "x" `Add` Var "y")
                                          ((Var "x" `Mult` Var "y'") `Add`
                                           (Var "x'" `Mult` Var "y"))))
            fDot     = Dot
            fTInt = TInt
            fTReal = TPair TReal TReal
            fTPair = TPair
            fTArray = TArray
            fTFun = TFun
