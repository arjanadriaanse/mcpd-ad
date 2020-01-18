module Algebra where
import qualified Data.Vector as V
import Language

type TypeAlgebra a = (
  a, -- TReal
  a, -- TInt
  a -> a, -- TArray
  a -> a -> a, -- TPair
  a -> a -> a, -- TFun
  a -- Unknown type
  )

type TermAlgebra a b = (
  Identifier  -> a, -- Var
  Double      -> a, -- CReal
  Int         -> a, -- CInt
  b -> V.Vector a -> a, -- CArray
  a -> a -> a, -- Pair
  b -> b -> Identifier -> a -> a, -- Fun
  a      -> a, -- Sigmoid  
  BinOp -> Maybe b -> a -> a -> a, -- BinOp  
  b -> a    -> a, -- New
  a           -> a, -- Length
  a -> a    -> a, -- Lookup
  a -> a -> a -> a, -- Update
  a -> a      -> a, -- Map
  a -> a -> a -> a, -- ZipWith
  a -> a -> a -> a, -- Fold  
  a -> Identifier -> Identifier -> a -> a, -- Case
  a -> a      -> a, -- Apply
  a -> a      -> a, -- Comp
  TypeAlgebra b
  )

idTypeAlgebra :: TypeAlgebra Type
idTypeAlgebra = (TReal, TInt, TArray, TPair, TFun, UnknownType)

foldType :: TypeAlgebra a -> Type -> a
foldType (fReal, fInt, fArray, fPair, fFun, fUnknown) = fType
  where
    fType TReal = fReal
    fType TInt = fInt
    fType (TPair t1 t2) = fPair (fType t1) (fType t2)
    fType (TArray t) = fArray (fType t)
    fType (TFun t1 t2) = fFun (fType t1) (fType t2)
    fType UnknownType  = fUnknown

foldTerm :: TermAlgebra a b -> Term -> a
foldTerm (fVar, fCReal, fCInt, fCArray, fPair, fFun, fSigmoid, fBinOp,
          fNew, fLength, fLookup, fUpdate, fMap, fZipWith,
          fFold, fCase, fApply, fComp, aType) = fTerm where
    fTerm (Var x)            = fVar   x 
    fTerm (CReal n)          = fCReal n
    fTerm (CInt n)           = fCInt  n 
    fTerm (CArray y ts)      = fCArray (fType y) (V.map fTerm ts)
    fTerm (Pair t1 t2)       = fPair (fTerm t1) (fTerm t2)
    fTerm (Fun y1 y2 x t)    = fFun  (fType y1) (fType y2) x (fTerm t)
    fTerm (Sigmoid t)        = fSigmoid  (fTerm t)
    fTerm (BinOp o y t1 t2)  = fBinOp o (fType <$> y) (fTerm t1) (fTerm t2)
    fTerm (New y t)          = fNew  (fType y)  (fTerm t)
    fTerm (Length t)         = fLength (fTerm t)
    fTerm (Lookup t1 t2)     = fLookup (fTerm t1) (fTerm t2)
    fTerm (Update t1 t2 t3)  = fUpdate (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Map t1 t2)        = fMap (fTerm t1) (fTerm t2)
    fTerm (ZipWith f t1 t2)  = fZipWith (fTerm f) (fTerm t1) (fTerm t2)
    fTerm (Fold t1 t2 t3)    = fFold (fTerm t1) (fTerm t2) (fTerm t3)
    fTerm (Case t1 x1 x2 t2) = fCase (fTerm t1) x1 x2 (fTerm t2)
    fTerm (Apply t1 t2)      = fApply (fTerm t1) (fTerm t2)
    fTerm (Comp f1 f2)       = fComp (fTerm f1) (fTerm f2)
    -- Algebra
    fType                = foldType aType
