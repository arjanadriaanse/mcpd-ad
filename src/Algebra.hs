module Algebra where
import qualified Data.Vector as V
import Language

type TypeAlgebra a = (
  a, -- TReal
  a, -- TInt
  a -> a, -- TArray
  a -> a -> a, -- TPair
  a -> a -> a -- TFun
  )

type TermAlgebra a b = (
  Identifier  -> a, -- Var
  Double      -> a, -- CReal
  Int         -> a, -- CInt
  b -> V.Vector a     -> a, -- CArray
  a -> a -> a, -- Pair
  b -> b -> Identifier -> a -> a, -- Fun
  a      -> a, -- Sigmoid  
  a -> a -> a, -- Add
  a -> a -> a, -- Mult
  a -> a -> a, -- Dot
  a -> a -> a, -- IntAdd
  a -> a -> a, -- IntMult  
  b -> a    -> a, -- New
  a           -> a, -- Length
  a -> a    -> a, -- Lookup
  a -> a -> a -> a, -- Update
  a -> a      -> a, -- Map
  a -> a -> a -> a, -- Fold  
  a -> Identifier -> Identifier -> a -> a, -- Case
  a -> a      -> a, -- Apply
  TypeAlgebra b
  )

idTypeAlgebra :: TypeAlgebra Type
idTypeAlgebra = (TReal, TInt, TArray, TPair, TFun)

foldType :: TypeAlgebra a -> Type -> a
foldType (fInt, fReal, fArray, fPair, fFun) = fType
  where
    fType TReal = fReal
    fType TInt = fInt
    fType (TPair t1 t2) = fPair (fType t1) (fType t2)
    fType (TArray t) = fArray (fType t)
    fType (TFun t1 t2) = fFun (fType t1) (fType t2)

foldTerm :: TermAlgebra a b -> Term -> a
foldTerm (fVar, fCReal, fCInt, fCArray, fPair, fFun, fSigmoid, fAdd, fMult,
          fDot, fIntAdd, fIntMult, fNew, fLength, fLookup, fUpdate, fMap,
          fFold, fCase, fApply, aType) = fTerm where
    fTerm (Var x)            = fVar  x 
    fTerm (CReal n)          = fCReal n
    fTerm (CInt n)           = fCInt  n 
    fTerm (CArray y ts)        = fCArray (fType y) (V.map fTerm ts)
    fTerm (Pair t1 t2)       = fPair (fTerm t1) (fTerm t2)
    fTerm (Fun y1 y2 x t)    = fFun  (fType y1) (fType y2) x (fTerm t)
    fTerm (Sigmoid t)    = fSigmoid  (fTerm t)
    fTerm (Add t1 t2)    = fAdd  (fTerm t1) (fTerm t2)
    fTerm (Mult t1 t2)   = fMult (fTerm t1) (fTerm t2)
    fTerm (Dot t1 t2)    = fDot  (fTerm t1) (fTerm t2)
    fTerm (IntAdd t1 t2) = fIntAdd  (fTerm t1) (fTerm t2)
    fTerm (IntMult t1 t2)= fIntMult (fTerm t1) (fTerm t2)
    fTerm (New y t)          = fNew  (fType y)  (fTerm t)
    fTerm (Length t)         = fLength  (fTerm t)
    fTerm (Lookup t1 t2)     = fLookup (fTerm t1) (fTerm t2)
    fTerm (Update t1 t2 t3)  = fUpdate   (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Map t1 t2)        = fMap (fTerm t1) (fTerm t2)
    fTerm (Fold t1 t2 t3)    = fFold (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Case t1 x1 x2 t2) = fCase (fTerm t1) x1 x2 (fTerm t2)
    fTerm (Apply t1 t2)      = fApply  (fTerm t1) (fTerm t2)
    -- Algebra
    fType                = foldType aType
