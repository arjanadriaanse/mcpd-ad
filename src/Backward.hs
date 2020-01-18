module Backward where 

import Language
import Algebra
import Variable
import qualified Data.Vector as V
import PrettyPrint


-- | Reverse mode algorithm 
differentiateB :: Int -> Term -> Term
differentiateB k = foldTerm
  (Var, fCReal, CInt, CArray, Pair, Fun, fSigmoid, fBinOp,
    New, Length, Lookup, Update, Map, ZipWith, Fold, Case, Apply, Comp,
   (fTReal, TInt, TArray, TPair, TFun, UnknownType)) . alphaRename where
  fTReal                                              = real $* (TFun real real)
  fCReal n                                            = CReal n $* (fun [("z", real)] ( CArray real (V.replicate k 0), TFun UnknownType real))
  fBinOp Add  (Just (TPair TReal (TFun _ TReal) )) t s = case_ t "x" "x'" $ case_ s "y" "y'" $
    (var "x" + var "y") $* (fun [("z", real)] ( var "x'" $$ var "z" + var "y'" $$ var "z", TFun UnknownType real ))
  fBinOp Mult (Just (TPair TReal (TFun _ TReal) )) t s = case_ t "x" "x'" $ case_ s "y" "y'" $
    (var "x" * var "y") $* (fun [("z",real)] (var "x'" $$ (var "y" * var "z") + var "y'" $$ (var "x" * var "z") , TFun UnknownType real)) 
  fBinOp Add  (Just TInt) t s                         = BinOp Add (Just TInt) t s
  fBinOp Mult (Just TInt) t s                         = BinOp Add (Just TInt) t s
  fBinOp Add  (Just (TArray (TPair TReal (TFun UnknownType TReal)))) t s = ZipWith addArray' t s 
  fBinOp Mult (Just (TArray (TPair TReal (TFun UnknownType TReal)))) t s = ZipWith multArray' t s 
  fBinOp Dot  (Just (TArray (TPair TReal (TFun UnknownType TReal)))) t s = error "todo" -- fold addTuple (0 $* 0 ) (ZipWith multArray' t s )
  fBinOp _ t _ _                                      = error ("operation not supported" ++ show t)
  fSigmoid t                                          = case_ t "x" "x'" $ let_ "y" (sigmoid (var "x"), real)
    (var "y" $* der , real $* (TFun real real)) where 
        der = fun [("z", real)] (var "x'" * (var "y" * (1 - var "y")) * var "z", TFun UnknownType real)

-- | Modified add/multiply for continuations
addArray' :: Term 
addArray' = fun [("t", real $* real), ("s", real $* real)] (
    case_ (var "t") "x" "x'" $ case_ (var "s") "y" "y'" $ 
    (var "x" + var "y") $* (fun [("z", real)] ( var "x'" $$ var "z" + var "y'" $$ var "z", TFun UnknownType real ))
    , real $* (TFun real real) )

multArray' :: Term 
multArray' = fun [("t", real $* real), ("s", real $* real)] (case_ (var "t") "x" "x'" $ case_ (var "s") "y" "y'" $
     (var "x" * var "y") $* (fun [("z",real)] (var "x'" $$ (var "y" * var "z") + var "y'" $$ (var "x" * var "z") , TFun UnknownType real)) 
     , real $* (TFun real real ))

addTuple :: Term
addTuple = fun [("x",real $* real), ("y", real $* real )] (case_ (var "x") "b0" "b1" $ 
        case_ (var "y") "c0" "c1" ((var "b0" + var "c0") $* (var "b1" + var "c1"))  , real $* real)


-- | Actually evaluating a reverse mode derivative representation
-- Unsure if this has type (real,real) (evR in paper)
evalReverse = fun [("result", real)] (case_ (var "result") "x" "x'" (var "x" $* (var "x'" $$ 1) ), real $* real)