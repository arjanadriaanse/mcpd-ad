-- The abstract machine v2
module AbstractMachine2 where

import Language
import PrettyPrint

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Stack = [Either Term Env]

data Mode = ModeEval | ModeReturn

data MachineState = MachineState Mode Stack Env Term

getRetTypeFun :: Term -> Type
getRetTypeFun (Closure _ (Fun _ (TFun _ t) _ _)) = t
getRetTypeFun (Closure _ (Fun _ t _ _))          = t

evaluate :: Env -> Term -> Term
evaluate env t = case transition' (MachineState ModeEval [] env t) of
                   MachineState ModeReturn [] _ t' -> t'
  where transition' s = case transition s of
          s'@(MachineState ModeReturn [] _ _) -> s'
          s'                                  -> transition' s'

transition :: MachineState -> MachineState
-- Constants
transition (MachineState ModeEval s env v@(CReal _)) = MachineState ModeReturn s env v
transition (MachineState ModeEval s env v@(CInt _))  = MachineState ModeReturn s env v
transition (MachineState ModeEval s env (CArray t vs)) = MachineState ModeReturn s env (CArray t (V.map (evaluate env) vs))

-- Variable
transition (MachineState ModeEval s env (Var x)) = MachineState ModeReturn s env (env M.! x)

-- Pair
transition (MachineState ModeEval s env (Pair t1 t2)) = MachineState ModeEval (Left (Pair Hole t2) : s) env t1
transition (MachineState ModeReturn (Left (Pair Hole t2) : s) env t1) = MachineState ModeEval (Left (Pair t1 Hole) : s) env t2
transition (MachineState ModeReturn (Left (Pair t1 Hole) : s) env t2) = MachineState ModeReturn s env (Pair t1 t2)

-- Function
transition (MachineState ModeEval s env f@(Fun _ _ _ _)) = MachineState ModeReturn s env (Closure env f)
transition (MachineState ModeEval s env f@(Closure _ _)) = MachineState ModeReturn s env f

-- Sigmoid
transition (MachineState ModeEval s env (Sigmoid t)) = MachineState ModeEval (Left (Sigmoid Hole) : s) env t
transition (MachineState ModeReturn (Left (Sigmoid Hole) : s) env (CReal v)) = MachineState ModeReturn s env (CReal (1 / (1 + exp (-v))))

-- BinOps
transition (MachineState ModeEval s env (BinOp o t t1 t2)) = MachineState ModeEval (Left (BinOp o t Hole t2) : s) env t1
transition (MachineState ModeReturn (Left (BinOp o t Hole t2) : s) env t1) = MachineState ModeEval (Left (BinOp o t t1 Hole) : s) env t2
transition (MachineState ModeReturn (Left (BinOp o t (CReal v1) Hole) : s) env (CReal v2)) =
  let vr = case o of
             Add  -> CReal (v1 + v2)
             Mult -> CReal (v1 * v2)
             _    -> error "Operation not supported on reals"
  in MachineState ModeReturn s env vr
transition (MachineState ModeReturn (Left (BinOp o t (CInt v1) Hole) : s) env (CInt v2)) =
  let vr = case o of
             Add  -> CInt (v1 + v2)
             Mult -> CInt (v1 * v2)
             _    -> error "Operation not supported on ints"
  in MachineState ModeReturn s env vr
transition (MachineState ModeReturn (Left (BinOp o _ (CArray t v1) Hole) : s) env (CArray _ v2)) =
  let f o (CReal n1) (CReal n2) = CReal (o n1 n2)
      vr = case o of
             Add  -> CArray t (V.zipWith (f (+)) v1 v2)
             Mult -> CArray t (V.zipWith (f (*)) v1 v2)
             Dot  -> V.foldl (f (+)) (CReal 0) (V.zipWith (f (*)) v1 v2)
  in if V.length v1 == V.length v2
     then MachineState ModeReturn s env vr
     else error "Combination of arrays with unequal lengths"

-- Array operations
transition (MachineState ModeEval s env (New t tl)) = MachineState ModeEval (Left (New t Hole) : s) env tl
transition (MachineState ModeReturn (Left (New t Hole) : s) env (CInt vl)) =
  let vr = case t of
             TReal -> CArray TReal (V.replicate vl (CReal 0))
             TInt  -> CArray TInt  (V.replicate vl (CInt 0))
             _     -> error $ "Cannot create new array of type: " ++ show t
  in MachineState ModeReturn s env vr
transition (MachineState ModeEval s env (Length t)) = MachineState ModeEval (Left (Length Hole) : s) env t
transition (MachineState ModeReturn (Left (Length Hole) : s) env (CArray _ v)) = MachineState ModeReturn s env (CInt (V.length v))
transition (MachineState ModeEval s env (Lookup t i)) = MachineState ModeEval (Left (Lookup Hole i) : s) env t
transition (MachineState ModeReturn (Left (Lookup Hole i) : s) env t) = MachineState ModeEval (Left (Lookup t Hole) : s) env i
transition (MachineState ModeReturn (Left (Lookup (CArray _ v) Hole) : s) env (CInt i)) = MachineState ModeReturn s env (v V.! i)
transition (MachineState ModeEval s env (Update t i v)) = MachineState ModeEval (Left (Update Hole i v) : s) env t
transition (MachineState ModeReturn (Left (Update Hole i v) : s) env t) = MachineState ModeEval (Left (Update t Hole v) : s) env i
transition (MachineState ModeReturn (Left (Update t Hole v) : s) env i) = MachineState ModeEval (Left (Update t i Hole) : s) env v
transition (MachineState ModeReturn (Left (Update (CArray t va) (CInt i) Hole) : s) env v) = MachineState ModeReturn s env (CArray t (va V.// [(i, v)]))
transition (MachineState ModeEval s env (Map f t)) = MachineState ModeEval (Left (Map Hole t) : s) env f
transition (MachineState ModeReturn (Left (Map Hole t) : s) env f) = MachineState ModeEval (Left (Map f Hole) : s) env t
transition (MachineState ModeReturn (Left (Map f Hole) : s) env (CArray t v)) = MachineState ModeEval s env (CArray (getRetTypeFun f) (V.map (f $$) v))
transition (MachineState ModeEval s env (ZipWith f t1 t2)) = MachineState ModeEval (Left (ZipWith Hole t1 t2) : s) env f
transition (MachineState ModeReturn (Left (ZipWith Hole t1 t2) : s) env f) = MachineState ModeEval (Left (ZipWith f Hole t2) : s) env t1
transition (MachineState ModeReturn (Left (ZipWith f Hole t2) : s) env t1) = MachineState ModeEval (Left (ZipWith f t1 Hole) : s) env t2
transition (MachineState ModeReturn (Left (ZipWith f (CArray t v1) Hole) : s) env (CArray _ v2)) = MachineState ModeEval s env (CArray (getRetTypeFun f) (V.zipWith (\x y -> f $$ x $$ y) v1 v2))
transition (MachineState ModeEval s env (Fold f v t)) = MachineState ModeEval (Left (Fold Hole v t) : s) env f
transition (MachineState ModeReturn (Left (Fold Hole v t) : s) env f) = MachineState ModeEval (Left (Fold f Hole t) : s) env v
transition (MachineState ModeReturn (Left (Fold f Hole t) : s) env v) = MachineState ModeEval (Left (Fold f v Hole) : s) env t
transition (MachineState ModeReturn (Left (Fold f v Hole) : s) env (CArray _ va)) =
  let g x y = f $$ x $$ y
  in MachineState ModeEval s env (V.foldr g v va)

-- Case
transition (MachineState ModeEval s env (Case tp x y tb)) = MachineState ModeEval (Left (Case Hole x y tb) : s) env tp
transition (MachineState ModeReturn (Left (Case Hole x y tb) : s) env (Pair t1 t2)) =
  let env' = M.insert x t1 $ M.insert y t2 env
  in MachineState ModeEval (Right env : s) env' tb

-- Function applications
transition (MachineState ModeEval s env (Apply t1 t2)) = MachineState ModeEval (Left (Apply Hole t2) : s) env t1
transition (MachineState ModeReturn (Left (Apply Hole t2) : s) env (Comp f1 f2)) = MachineState ModeEval (Left (Apply (Comp f1 Hole) Hole) : s) env (f2 $$ t2)
transition (MachineState ModeReturn (Left (Apply (Comp f1 Hole) Hole) : s) env t2) = MachineState ModeEval s env (f1 $$ t2)
transition (MachineState ModeReturn (Left (Apply Hole t2) : s) env t1) = MachineState ModeEval (Left (Apply t1 Hole) : s) env t2
transition (MachineState ModeReturn (Left (Apply (Closure env' (Fun _ _ x tb)) Hole) : s) env t2) =
  let env'' = M.insert x t2 env'
  in MachineState ModeEval (Right env : s) env'' tb
transition (MachineState ModeEval s env (Comp t1 t2)) = MachineState ModeReturn s env (Comp t1 t2)

-- Returning values with environment on stack
transition (MachineState ModeReturn (Right env : s) _ v) = MachineState ModeReturn s env v

-- Terms that cannot be evaluated to values
transition (MachineState _ _ _ t) = error $ "Term cannot be evaluated to value: " ++ show t

