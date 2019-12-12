module Algebra where

import Language

type TermAlgebra t = (
    Identifier  -> t, -- This is the Var
    Double      -> t, -- CReal
    Int         -> t, -- CInt
    Type -> t   -> t, -- New
    t -> t      -> t, -- Lookup
    t -> t -> t -> t, -- Update
    t           -> t, -- Length
    t -> t      -> t, -- Map
    t -> t -> t -> t, -- Fold
    Type -> Type -> Identifier -> t -> t, -- Fun
    t -> t      -> t, -- Application

    -- Operators
    t      -> t, -- Neg
    t      -> t, -- Sigmoid  
    t -> t -> t, -- Add
    t -> t -> t, -- Mult
    t -> t -> t  -- Dot product
    )


foldTerm :: TermAlgebra a -> Term -> a
foldTerm (fVar, fReal, fInt, fNew, fLook, fUp, fLen, fMap, 
            fFold, fFun, fApp, fNeg, fSig, fAdd, fMult, fDot) = fTerm where
    fTerm (Var x)        = fVar x 
    fTerm (CReal n)      = fReal n
    fTerm (CInt n)       = fInt n 
    fTerm (New y t)      = fNew y (fTerm t)
    fTerm (Lookup t1 t2) = fLook  (fTerm t1) (fTerm t2)
    fTerm (Update t1 t2 t3) = fUp (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Length t)     = fLen   (fTerm t)
    fTerm (Map t1 t2)    = fMap   (fTerm t1) (fTerm t2)
    fTerm (Fold t1 t2 t3)= fFold  (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Fun y1 y2 x t)= fFun y1 y2 x (fTerm t)
    fTerm (FunApp t1 t2) = fApp   (fTerm t1) (fTerm t2)
    -- Operators
    fTerm (Neg t)        = fNeg  (fTerm t)
    fTerm (Sigmoid t)    = fSig  (fTerm t)
    fTerm (Add t1 t2)    = fAdd  (fTerm t1) (fTerm t2)
    fTerm (Mult t1 t2)   = fMult (fTerm t1) (fTerm t2)
    fTerm (Dot t1 t2)    = fDot  (fTerm t1) (fTerm t2)


    
