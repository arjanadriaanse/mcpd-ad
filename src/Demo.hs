module Demo where

import Language
import Forward
import PrettyPrint
import StaticCheck
import AbstractMachine
import Annotate
import Test

import qualified Data.Vector as V
import qualified Data.Map as M

exampleParabola :: Term
exampleParabola = fun [("x", real)] (var "x" * var "x", real)

examplePolyMultiVar :: Term
examplePolyMultiVar = fun [("x", real), ("y", real), ("z", real)] (var "x" * var "y" * var "z" + 2 * (var "x" $^ 2) * var "y" + (var "z" $^ 3), real)

exampleLogReg :: Term
exampleLogReg = fun [("x", array real), ("w", array real), ("b", real)] (sigmoid (var "x" `dot` var "w" + var "b"), real)

exampleArray1a :: Term
exampleArray1a = map_ (addC real $$ 0.1) (new real (CInt 10))
exampleArray1b :: Term
exampleArray1b = update (new real (CInt 10)) (CInt 1) 1
exampleArray1 :: Term
exampleArray1 = zipwith (pairC real real) exampleArray1a exampleArray1b

exampleArray2 :: Term
exampleArray2 = zipwith (pairC real real) (update (exampleArray1a + exampleArray1a) (CInt 1) (-1)) (new real (CInt 10))

addC :: Type -> Term
addC t = fun [("x", t), ("y", t)] (var "x" + var "y", t)

pairC :: Type -> Type -> Term
pairC t1 t2 =  fun [("x", t1), ("y", t2)] (var "x" $* var "y", t1 $* t2)

layer :: Int -> Term
layer n = fun [("f", TFun (array real) (TFun (array real) (TFun real real))), ("ws", array (array real)), ("bs", array real), ("x", array real)] (CArray real (V.fromList $ map g [0 .. n-1]), array real)
  where g i = var "f" $$ var "x" $$ lookup_ (var "ws") (CInt i) $$ lookup_ (var "bs") (CInt i)

neuralnet1 :: Term
neuralnet1 = fun [("x", array real), ("ws1", array (array real)), ("bs1", array real), ("ws2", array (array real)), ("bs2", array real), ("ws3", array (array real)), ("bs3", array real)]
                 (((layer 1 $$ f $$ var "ws3" $$ var "bs3") $. (layer 4 $$ f $$ var "ws2" $$ var "bs2") $. (layer 3 $$ f $$ var "ws1" $$ var "bs1")) $$ var "x", array real)
  where f = exampleLogReg

exampleVectorField :: Term
exampleVectorField = fun [("x", real)] (CArray real (V.fromList [var "x", var "x" * var "x", var "x" * var "x" * var "x"]), array real)

{- script

exampleParabola
evaluate $ exampleParabola $$ 4
d = differentiate . annotate $ exampleParabola
d
evaluate $ d $$ (4 $* 1)

examplePolyMultiVar
typecheck M.empty examplePolyMultiVar
d = differentiate . annotate $ examplePolyMultiVar
d
typecheck M.empty d
evaluate $ d $$ (3 $* 1) $$ (4 $* 0) $$ (5 $* 0)
finiteDifferences examplePolyMultiVar [3 $* 1, 4 $* 0, 5 $* 0]

exampleLogReg
d = differentiate . annotate $ exampleLogReg
d

-}
