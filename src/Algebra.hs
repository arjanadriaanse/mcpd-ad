module Algebra where

import Language

type TypeAlgebra t = (
  t,
  t,
  t -> t -> t,
  t -> t,
  t -> t -> t
  )

type TermAlgebra t tau = (
    Identifier  -> t, -- This is the Var
    Double      -> t, -- CReal
    Int         -> t, -- CInt

    -- Pair and case
    t -> t -> t, -- Pair
    t -> Identifier -> Identifier -> t -> t,

    tau -> t    -> t, -- New
    t -> t      -> t, -- Lookup
    t -> t -> t -> t, -- Update
    t           -> t, -- Length
    t -> t      -> t, -- Map
    t -> t -> t -> t, -- Fold
    tau -> tau -> Identifier -> t -> t, -- Fun
    t -> t      -> t, -- Application

    -- Operators
    t      -> t, -- Sigmoid  
    t -> t -> t, -- Add
    t -> t -> t, -- Mult
    t -> t -> t, -- Dot product
    TypeAlgebra tau
    )

idTypeAlgebra :: TypeAlgebra Type
idTypeAlgebra = (TInt, TReal, TPair, TArray, TFun)

foldType :: TypeAlgebra a -> Type -> a
foldType (fInt, fReal, fPair, fArray, fFun) = fType
  where
    fType TInt = fInt
    fType TReal = fReal
    fType (TPair t1 t2) = fPair (fType t1) (fType t2)
    fType (TArray t) = fArray (fType t)
    fType (TFun t1 t2) = fFun (fType t1) (fType t2)

foldTerm :: TermAlgebra a b -> Term -> a
foldTerm (fVar, fReal, fInt, fPair, fCase, fNew, fLook, fUp, fLen, fMap, 
            fFold, fFun, fApp, fSig, fAdd, fMult, fDot, aType) = fTerm where
    fTerm (Var x)            = fVar  x 
    fTerm (CReal n)          = fReal n
    fTerm (CInt n)           = fInt  n 
    -- Pair Case
    fTerm (Pair t1 t2)       = fPair (fTerm t1) (fTerm t2)
    fTerm (Case t1 x1 x2 t2) = fCase (fTerm t1) x1 x2 (fTerm t2)
    fTerm (New y t)          = fNew  (fType y)  (fTerm t)
    fTerm (Lookup t1 t2)     = fLook (fTerm t1) (fTerm t2)
    fTerm (Update t1 t2 t3)  = fUp   (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Length t)         = fLen  (fTerm t)
    fTerm (Map t1 t2)        = fMap  (fTerm t1) (fTerm t2)
    fTerm (Fold t1 t2 t3)    = fFold (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Fun y1 y2 x t)    = fFun  (fType y1) (fType y2) x (fTerm t)
    fTerm (FunApp t1 t2)     = fApp  (fTerm t1) (fTerm t2)
    -- Operators
    fTerm (Sigmoid t)    = fSig  (fTerm t)
    fTerm (Add t1 t2)    = fAdd  (fTerm t1) (fTerm t2)
    fTerm (Mult t1 t2)   = fMult (fTerm t1) (fTerm t2)
    fTerm (Dot t1 t2)    = fDot  (fTerm t1) (fTerm t2)
    -- Algebra
    fType                = foldType aType
