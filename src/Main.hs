module Main where

import Language
import Forward
import PrettyPrint
import StaticCheck
import Variable
import FoldMachine
import qualified StackMachine as A2
import Annotate
import Numerical hiding (evaluate)

import qualified Data.Map as M
import qualified Data.Vector as V

main :: IO()
main = putStrLn "Run differentiateWrapper on anything!"

-- | Examples calling with 0 or more arguments
callDiffWrapper  = differentiateWrapper examplePolyMultiVar  [(5 $* 1), (8 $* 0), (1000 $* 0)] 
callDiffWrapper2 = differentiateWrapper exampleDot []
callDiffWrapper3 = differentiateWrapper exampleComp [4 $* 1]
callDiffWrapper4 = differentiateWrapper exampleLogReg [exampleArray1, exampleArray2, 0.5 $* 0 ]

-- | Takes a function, and a list of tuples which are the functions arguments 
-- | Applies derivative of exp to all inputs xs 
differentiateWrapper :: Term -> [Term] -> IO ()
differentiateWrapper exp xs = do
    putStrLn ("--------------------------------")
    putStrLn ("Performing initial typecheck")
    case typecheck M.empty exp of
      Left e  -> putStrLn ("Error: " ++ show e)
      Right _ -> do
        putStrLn ("Ok")
        putStrLn ("--------------------------------")
        putStrLn ("Evaluating " ++ show exp)
        putStrLn ("Arguments: " ++ show xs)
        putStrLn ("-----Derivative expression------")
        putStrLn $ show diff
        putStrLn ("-----Evaluating...--------------")
        case typecheck M.empty exp' of
          Left e  -> putStrLn ("Error: " ++ show e)
          Right _ -> do
            putStrLn ("Second typecheck OK")
            putStrLn ("-----Exact derivative-----------")
            putStrLn $ show result 
            putStrLn ("-----Approximate derivative-----")
            putStrLn $ show fd 
            putStrLn ("--------------------------------")
  where 
    exp'   = foldl ($$) diff xs
    result = A2.evaluate M.empty exp'
    diff   = (differentiate . annotate) exp
    fd     = finiteDifferences exp xs

examplePoly1 :: Term
examplePoly1 = fun [("y", real)] (var "y" * var "y" + var "y", real)

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
exampleNested = fun [("x", real),("y", real), ("z", real)] (var "x" * (var "y" + var "z"), real)

exampleStructurePreservingArrays :: Term 
exampleStructurePreservingArrays = update array (CInt 4) 5.5
    where 
        array = new TReal (CInt 10)

exampleIntArray :: Term 
exampleIntArray = update array (CInt 0) (length_ array) where 
    array = CArray int (V.fromList [0,1,2,3,4,5,10])

realpair = real $* real

exampleFst :: Term
exampleFst = fun [("x", realpair)] ( case_ (var "x") "id1" "id2" (var "id1") , real) 

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

exampleInput :: Term
exampleInput = exampleArray1a

exampleInputad :: Term
exampleInputad = zipwith (pairC real real) exampleInput (new real (length_ exampleInput))

exampleWeights1 :: Term
exampleWeights1 = CArray (array real) (V.fromList [exampleArray1a, exampleArray1a, exampleArray1a])

exampleWeights1ad :: Term
exampleWeights1ad = CArray (array (real $* real)) (V.fromList [exampleInputad, exampleInputad, exampleInputad])

exampleOffset1 :: Term
exampleOffset1 = CArray real (V.fromList [0.3, 0.1, 0.2])

exampleOffset1ad :: Term
exampleOffset1ad = zipwith (pairC real real) exampleOffset1 (CArray real (V.fromList [0, 1, 0]))

exampleWeights2a :: Term
exampleWeights2a = CArray real (V.fromList [0.2, 0.1, 0.6])

exampleWeights2aa :: Term
exampleWeights2aa = zipwith (pairC real real) exampleWeights2a (new real (length_ exampleWeights2a))

exampleWeights2 :: Term
exampleWeights2 = CArray (array real) (V.fromList [exampleWeights2a, exampleWeights2a, exampleWeights2a, exampleWeights2a])    

exampleWeights2ad :: Term
exampleWeights2ad = CArray (array (real $* real)) (V.fromList [exampleWeights2aa, exampleWeights2aa, exampleWeights2aa, exampleWeights2aa]) 

exampleOffset2 :: Term
exampleOffset2 = CArray real (V.fromList [0.2, 0.1, 0.5, 0.1])

exampleOffset2ad :: Term
exampleOffset2ad = zipwith (pairC real real) exampleOffset2 (new real (length_ exampleOffset2))

exampleWeights3a :: Term
exampleWeights3a = CArray real (V.fromList [0.7, 0.1, 0.6, 0.2])

exampleWeights3aa :: Term
exampleWeights3aa = zipwith (pairC real real) exampleWeights3a (new real (length_ exampleWeights3a))

exampleWeights3 :: Term
exampleWeights3 = CArray (array real) (V.fromList [exampleWeights3a])

exampleWeights3ad :: Term
exampleWeights3ad = CArray (array (real $* real)) (V.fromList [exampleWeights3aa])

exampleOffset3 :: Term
exampleOffset3 = CArray real (V.fromList [0.1])

exampleOffset3ad :: Term
exampleOffset3ad = zipwith (pairC real real) exampleOffset3 (new real (length_ exampleOffset3))


