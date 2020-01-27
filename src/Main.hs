module Main where

import Language
import Forward
import PrettyPrint
import StaticCheck
import Variable
import AbstractMachine
import Annotate
import Test

import qualified Data.Map as M
import qualified Data.Vector as V

main :: IO ()
main = putStrLn "Hello, Haskell!"

example :: Term
example = df $$ (10 $* 1)
    where
        df = differentiate $ fun [("y", real)] (var "y" * var "y" + var "y", real)

exampleNest :: Term
exampleNest = fun [("y", real)] (f $$ 20, real) $$ 8000
    where
        f = fun [("y", real)] (var "y" + var "y", real)

exampleFst :: Term
exampleFst = fun [("x", TPair real real)] ( case_ (var "x") "id1" "id2" (var "id1") , real) $$ (Pair 10 20)

exampleMap :: Term
exampleMap = map_ (fun [("x", real)] (var "x" + 20, real)) (new real (CInt 10))

exampleDot :: Term
exampleDot = dot exampleMap exampleMap

newIndexList :: Term
newIndexList = update (new real (CInt 10)) (CInt 1) (5.1 + 9)

examplef :: Term
examplef = fold f 0 exampleMap where
    f = fun [("b", real), ("x", real)] (var "x" + var "b", real)

exampleNested :: Term
exampleNested = f $$ 10 $$ 5 $$ 3
    where
        f = fun [("x", real),("y", real), ("z", real)] (var "x" * (var "y" + var "z"), real)

-- 9x + 3x^3
-- 9  + 9x^2
-- 18x

realpair = real $* real
exampleSnd :: Term
exampleSnd = fun [("pair", TPair realpair realpair)] ( case_ (var "pair") "id1" "id2" (var "id2") , realpair)

exampleParabola :: Term
exampleParabola = fun [("x", real)] (var "x" * var "x", real)

examplePoly :: Term
examplePoly = fun [("x", real)] ( (3 * (var "x" * var "x" * var "x")) + var "x" * 9 ,real)

examplePolyMultiVar :: Term
examplePolyMultiVar = fun [("x", real), ("y", real), ("z", real)] (var "x" * var "y" * var "z" + 2 * (var "x" $^ 2) * var "y" + (var "z" $^ 3), real)

exampleLogReg :: Term
exampleLogReg = fun [("x", array real), ("w", array real), ("b", real)] (sigmoid (var "x" `dot` var "w" + var "b"), real)

-- | How to do function composition without polymorphism?

exampleSecondDerivative :: Term
exampleSecondDerivative =  (fun [("x", realpair)] (exampleSnd $$ ((differentiate examplePoly) $$ (var "x")), real)) -- ?? How to do it

exampleFromPaper :: Term
exampleFromPaper = fun [("x", real), ("y", real)] (snd_ real real $$ inner, real) where
    inner    = ((differentiate . annotate) innerfun) $$ ((var "x") $* 1)
    innerfun = (var "x") * ( snd_ real real $$ dfy )
    dfy      = ((differentiate . annotate) (var "x" + var "y")) $$ ((var "y") $* 1)


exampleZip :: Term
exampleZip = zipwith f exampleMap exampleMap where
    f = fun [("x", real), ("y", real)] ( var "x" - (2 * var "y") , real )

exampleComp :: Term
exampleComp = comp where
    comp = f1 $. f2
f1 = fun [("x", real)] (var "x" + 9, real )
f2 = fun [("y", real)] (var "y" * 2, real )

exampleCompFromMain = comptesting f1 f2

comptesting :: Term -> Term -> Term
comptesting func2 func1 = case (func1, func2) of
          ((Fun tf11 tf12 id1 body1), (Fun tf21 tf22 id2 body2)) -> result where
              result = Fun tf11 tf22 id1 body
              body   = let_ id2 (body1, tf12) (body2, tf22)


exampleElementWise :: Term
exampleElementWise = exampleMap + exampleMap

exampleElementWise2 :: Term
exampleElementWise2 = 1 - (exampleMap * 0.01)

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


-- | Structural examples of every kind of expression in our language

-- lookup example, length example, the rest is inc. 
exampleStructurePreservingArrays :: Term 
exampleStructurePreservingArrays = update array (CInt 4) 5.5
    where 
        array = new TReal (CInt 10)

exampleIntArray :: Term 
exampleIntArray = update array (CInt 0) (length_ array) where 
    array = CArray int (V.fromList [0,1,2,3,4,5,10])
