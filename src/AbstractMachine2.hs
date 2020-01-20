-- The abstract machine v2
module AbstractMachine2 where

import Language

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Stack = [Either Term Env]

data Mode = ModeEval | ModeReturn

data MachineState = MachineState Mode Stack Env Term

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

-- Sigmoid
transition (MachineState ModeEval s env (Sigmoid t)) = MachineState ModeEval (Left (Sigmoid Hole) : s) env t
transition (MachineState ModeReturn (Left (Sigmoid Hole) : s) env (CReal v)) = MachineState ModeReturn s env (CReal (1 / (1 + exp (-v))))

-- BinOps
