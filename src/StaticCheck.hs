module StaticCheck where

import Language
import PrettyPrint -- TODO: REMOVE AFTER ADDING SHOW INSTANCE FOR TYPEERROR

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad

type Env = M.Map String Type

-- | Set of types that are supported by the binary operators
binOpTypes :: S.Set Type
binOpTypes = S.fromList [TReal, TArray TReal, TInt]

-- | Possible errors that can occur during the type checking.
data TypeError = TypeMismatch Type Type -- Expected type vs Actual type
               | NonArray Type
               | NonFunction Type
               | NonPair Type
               | UnknownVariable Identifier
               | UnsupportedBinOpType Type
               deriving Show

-- | Compares two types and returns an error if they are not the same
unify :: Type -> Type -> Either TypeError Type
unify tau1 tau2 | tau1 == tau2 = Right tau1
                | otherwise    = Left $ TypeMismatch tau1 tau2

-- | Helper function for performing the type check of binary operator terms
binOpCheck :: S.Set Type -> Env -> Term -> Term -> Either TypeError Type
binOpCheck s e t1 t2 = do
  tau1 <- typecheck e t1
  tau2 <- typecheck e t2
  tau  <- unify tau1 tau2
  if tau `S.member` s
  then Right tau
  else Left $ UnsupportedBinOpType tau

-- | Performs a type check on the given term under the given type environment
--   (containing variables with their type)
typecheck :: Env -> Term -> Either TypeError Type
typecheck e (Var x) = case M.lookup x e of
                        Just tau -> Right tau
                        _        -> Left $ UnknownVariable x
typecheck _ (CReal _) = Right TReal
typecheck _ (CInt _)  = Right TInt
typecheck e (CArray tau ts) = traverse (typecheck e) ts >>= foldM unify tau
typecheck e (Pair t1 t2) = TPair <$> typecheck e t1 <*> typecheck e t2
typecheck e (Fun tau1 tau2 x t) = do
  tauBody <- typecheck (M.insert x tau1 e) t
  if tau2 == tauBody
  then Right $ TFun tau1 tau2
  else Left  $ TypeMismatch tau2 tauBody
typecheck e (Sigmoid t)  = typecheck e t >>= unify TReal
typecheck e (BinOp Add _ t1 t2)  = binOpCheck binOpTypes e t1 t2
typecheck e (BinOp Mult _ t1 t2) = binOpCheck binOpTypes e t1 t2
typecheck e (BinOp Dot _ t1 t2)  = binOpCheck (S.singleton (TArray TReal)) e t1 t2 >> Right TReal
typecheck e (New tau t) = do
  tauL <- typecheck e t
  unify TInt tauL
  Right $ TArray tau
typecheck e (Length t) = do
  tauA <- typecheck e t
  case tauA of
    TArray _ -> Right TInt
    _        -> Left $ NonArray tauA
typecheck e (Lookup t i) = do
  tauI <- typecheck e i
  unify TInt tauI
  tauA <- typecheck e t
  case tauA of
    TArray tau -> Right tau
    _          -> Left $ NonArray tauA
typecheck e (Update t i v) = do
  tauI <- typecheck e i
  unify TInt tauI
  tauA <- typecheck e t
  case tauA of
    TArray tau -> typecheck e v >>= unify tau >> Right tauA
    _          -> Left $ NonArray tauA
typecheck e (Map f t) = do
  tauF <- typecheck e f
  case tauF of
    TFun tau1 tau2 -> typecheck e t >>= unify (TArray tau1) >> Right (TArray tau2)
    _              -> Left $ NonFunction tauF
typecheck e (Fold f v t) = do
  tauF <- typecheck e f
  case tauF of
    TFun tau1 (TFun tau2 tau3) -> do unify tau2 tau3
                                     tauA <- typecheck e t
                                     unify (TArray tau1) tauA
                                     tauV <- typecheck e v
                                     unify tau2 tauV
    _                          -> Left $ NonFunction tauF
typecheck e (Case t1 x1 x2 t2) = do
  tauP <- typecheck e t1
  case tauP of
    TPair tau1 tau2 -> let e' = M.insert x1 tau1 $ M.insert x2 tau2 e
                       in typecheck e' t2
    _               -> Left $ NonPair tauP
typecheck e (Apply t1 t2) = do
  tauF <- typecheck e t1
  case tauF of
    TFun tau1 tau2 -> typecheck e t2 >>= unify tau1 >> Right tau2
    _              -> Left $ NonFunction tauF
