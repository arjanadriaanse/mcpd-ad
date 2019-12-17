module Algebra where

import Language

type TypeAlgebra a = (
  a,
  a,
  a -> a -> a,
  a -> a,
  a -> a -> a
  )

type TermAlgebra a b = (
  Identifier  -> a, -- Var
  Double      -> a, -- CReal
  Int         -> a, -- CInt
  
  -- Pair and case
  a -> a -> a, -- Pair
  a -> Identifier -> Identifier -> a -> a,
  b -> b -> Identifier -> a -> a, -- Fun
  a -> a      -> a, -- Apply
  
  b -> a    -> a, -- New
  a           -> a, -- Length
  a -> a    -> a, -- Lookup
  a -> a -> a -> a, -- Update
  a -> a      -> a, -- Map
  a -> a -> a -> a, -- Fold
  
  -- Operators
  a      -> a, -- Sigmoid  
  a -> a -> a, -- Add
  a -> a -> a, -- Mult
  a -> a -> a, -- Dot product
  a -> a -> a, -- IntAdd
  a -> a -> a, -- IntMult
  TypeAlgebra b
  )

idTypeAlgebra :: TypeAlgebra Type
idTypeAlgebra = (TReal, TInt, TPair, TArray, TFun)

foldType :: TypeAlgebra a -> Type -> a
foldType (fInt, fReal, fPair, fArray, fFun) = fType
  where
    fType TReal = fReal
    fType TInt = fInt
    fType (TPair t1 t2) = fPair (fType t1) (fType t2)
    fType (TArray t) = fArray (fType t)
    fType (TFun t1 t2) = fFun (fType t1) (fType t2)

foldTerm :: TermAlgebra a b -> Term -> a
foldTerm (fVar, fReal, fInt, fPair, fCase, fFun, fApp, fNew, fLen, fLook, fUp, fMap, 
            fFold, fSig, fAdd, fMult, fDot, fIntAdd, fIntMult, aType) = fTerm where
    fTerm (Var x)            = fVar  x 
    fTerm (CReal n)          = fReal n
    fTerm (CInt n)           = fInt  n 
    -- Pair Case
    fTerm (Pair t1 t2)       = fPair (fTerm t1) (fTerm t2)
    fTerm (Case t1 x1 x2 t2) = fCase (fTerm t1) x1 x2 (fTerm t2)
    fTerm (Fun y1 y2 x t)    = fFun  (fType y1) (fType y2) x (fTerm t)
    fTerm (Apply t1 t2)      = fApp  (fTerm t1) (fTerm t2)
    fTerm (New y t)          = fNew  (fType y)  (fTerm t)
    fTerm (Length t)         = fLen  (fTerm t)
    fTerm (Lookup t1 t2)     = fLook (fTerm t1) (fTerm t2)
    fTerm (Update t1 t2 t3)  = fUp   (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Map t1 t2)        = fMap  (fTerm t1) (fTerm t2)
    fTerm (Fold t1 t2 t3)    = fFold (fTerm t1) (fTerm t2) (fTerm t3)
    -- Operators
    fTerm (Sigmoid t)    = fSig  (fTerm t)
    fTerm (Add t1 t2)    = fAdd  (fTerm t1) (fTerm t2)
    fTerm (Mult t1 t2)   = fMult (fTerm t1) (fTerm t2)
    fTerm (Dot t1 t2)    = fDot  (fTerm t1) (fTerm t2)
    fTerm (IntAdd t1 t2) = fIntAdd  (fTerm t1) (fTerm t2)
    fTerm (IntMult t1 t2)= fIntMult (fTerm t1) (fTerm t2)
    -- Algebra
    fType                = foldType aType
