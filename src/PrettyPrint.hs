module PrettyPrint where 
import Language
import Algebra
-- Todo intercalate

instance Show Type where
    show TInt         = "int"
    show TReal        = "real"
    show (TArray y)   = "[" ++ show y ++ "]" 
    show (TFun y1 y2) = show y1 ++ " -> " ++ show y2
    show (TPair y1 y2) = "<" ++ show y1 ++ ", " ++ show y2 ++ ">"

instance Show Term where
    show = foldTerm (fVar, fReal, fInt, fPair, fCase, fNew, fLook, fUp, fLen, fMap, 
        fFold, fFun, fApp, fSig, fAdd, fMult, fDot,
        idTypeAlgebra) where
            fVar  = id
            fReal = show 
            fInt  = show

            -- Pair, case
            fPair t1 t2       = "<" ++ t1 ++ ", " ++ t2 ++ ">"
            fCase t1 x1 x2 t2 = "case " ++ t1 ++ " of " ++ (fPair x1 x2 ) ++ " -> " ++ t2
            
            -- The rest
            fNew y size  = ("new " ++ show y) ++ " " ++ size 
            fLook t1 t2  = t1 ++ "[" ++ t2 ++ "]"
            fUp t1 t2 t3 = t1 ++ "[" ++ t2 ++ "] := " ++ t3
            fLen t1      = "len " ++ t1 ++ ""
            fMap t1 t2   = "map " ++ t1 ++ " " ++ t2
            fFold t1 t2 t3 = "fold " ++ t1 ++ " " ++ t2 ++ " " ++ t3
            fFun y1 y2 x t = "fun : (" ++ show (TFun y1 y2) ++ ") " ++ x ++ " . " ++ t
            fApp t1 t2     = t1 ++ " " ++ t2
            
            -- Operators
            fSig t         = "sigmoid(" ++ t ++ ")"
            fAdd t1 t2     = t1 ++ "+" ++ t2
            fMult t1 t2    = t1 ++ "*" ++ t2
            fDot t1 t2     = t1 ++ "@" ++ t2
            

example :: Term
example = Fun TReal TReal "x" (Mult (Var "x") (Var "x"))

exampleFst :: Term
exampleFst = Fun (TPair TReal TReal) (TReal) "x" (Case (Var "x") "id1" "id2" (Var "id1") )

