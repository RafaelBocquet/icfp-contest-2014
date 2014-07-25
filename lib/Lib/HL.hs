module Lib.HL where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable (foldl', foldrM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Type = 
    TInt
  | TFunc Type Type
  | TList Type
  | TTuple [Type]
  | TVar String
  | TOther
  deriving(Eq)

isFunctionType :: Type -> Bool
isFunctionType (TFunc {}) = True
isFunctionType _          = False

instance Show Type where
  show TInt             = "I"
  show (TVar s)         = s
  show (TFunc a b)
    | isFunctionType a  = "(" ++ show a ++ ")" ++ " -> " ++ show b
    | otherwise         = show a ++ " -> " ++ show b
  show (TList a)        = "[" ++ show a ++ "]"
  show (TTuple [])      = "()"
  show (TTuple (t:ts))  = "(" ++ show t ++ (concat $ ((", " ++) . show) <$> ts)  ++ ")"
  show TOther           = "?"

lambdaManStatusType :: Type
lambdaManStatusType = TTuple
  [ TInt
  , TTuple [TInt, TInt]
  , TInt
  , TInt
  , TInt
  ]

ghostStatusType :: Type
ghostStatusType = TTuple
  [ TInt
  , TTuple [TInt, TInt]
  , TInt
  ]

worldType :: Type
worldType = TTuple
  [ TList (TList TInt)
  , lambdaManStatusType
  , TList ghostStatusType
  , TInt
  ]

stepClosureType :: Type -> Type
stepClosureType state = TFunc state $ TFunc worldType $ TTuple [state, TInt]

mainType :: Type -> Type
mainType state = TFunc worldType $ TFunc TOther $ TTuple [state, stepClosureType state]

data Expr =
    EConst Int
  | EVar String
  | ELet String Expr Expr
  | EType String Type Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EIf0 Expr Expr Expr
  | ELambda String Type Expr
  | EApp Expr Expr
  | ETuple [Expr]
  | ETupleGet Expr Int

instance Show Expr where
  show (EConst i)      = show i
  show (EVar s)        = s
  show (ELet n v e)    = "let " ++ n ++ " = " ++ show v ++ " in " ++ show e
  show (EType n v e)   = "let " ++ n ++ " = " ++ show v ++ " in " ++ show e
  show (EAdd e1 e2)    = "(" ++ show e1 ++ ")" ++ " + " ++ "(" ++ show e2 ++ ")"
  show (ESub e1 e2)    = "(" ++ show e1 ++ ")" ++ " - " ++ "(" ++ show e2 ++ ")"
  show (EMul e1 e2)    = "(" ++ show e1 ++ ")" ++ " * " ++ "(" ++ show e2 ++ ")"
  show (EDiv e1 e2)    = "(" ++ show e1 ++ ")" ++ " / " ++ "(" ++ show e2 ++ ")"
  show (EIf0 c b1 b2)  = "if0 " ++ show c ++ " then " ++ show b1 ++ " else " ++ show b2
  show (ELambda n t e) = "\\" ++ n ++ " : " ++ show t ++ ". " ++ show e
  show (EApp f t)      = "(" ++ show f ++ ")" ++ show t
  show (ETupleGet e i) = "(" ++ show e ++ ")" ++ "[" ++ show i ++ "]"
  show (ETuple [])     = "()"
  show (ETuple (e:es)) = "(" ++ show e ++ (concat $ ((", " ++) . show) <$> es)  ++ ")"

data Error =
    DuplicateVariable String
  | UnknownVariable String
  | ArithNotInt
  | ConditionalNotInt
  | IfBranchNotSameType
  | NonFunctionApplication
  | FunctionTypeMismatch
  | BadTupleIndex
  | ExceptedTuple
  | UnknownTypeVariable String
  deriving(Show)

substituteType :: Map String Type -> Type -> Either Error Type
substituteType mp (TInt) = return TInt
substituteType mp (TVar s) = return TInt
substituteType mp (TOther) = return TOther
substituteType mp (TFunc f t) = do
  f' <- substituteType mp f
  t' <- substituteType mp t
  return $ TFunc f' t'
substituteType mp (TList t) = TList <$> substituteType mp t
substituteType mp (TTuple ts) =
  TTuple <$> foldrM (\t ts' -> do
      t' <- substituteType mp t
      return $ t':ts'
    ) [] ts

typecheck :: (Map String Type, Map String Type) -> Expr -> Either Error Type
typecheck (me, mt) e = do
  eTy <- typecheck' (me, mt) e
  substituteType mt eTy
  where
    typecheck' :: (Map String Type, Map String Type) -> Expr -> Either Error Type
    typecheck' (me, mt) expr@(EConst _)     = return TInt
    typecheck' (me, mt) expr@(EVar s)       = case Map.lookup s me of
      Nothing -> throwError $ UnknownVariable s
      Just x  -> return x
    typecheck' (me, mt) expr@(ELet n v e)   =
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable n
        Nothing -> do
          vTy <- typecheck (me, mt) v
          typecheck (Map.insert n vTy me, mt) e
    typecheck' (me, mt) expr@(EType n v e)   = do
      v' <- substituteType mt v
      case Map.lookup n mt of
        Just _ -> throwError $ DuplicateVariable n
        Nothing -> typecheck (me, Map.insert n v' mt) e
    typecheck' (me, mt) expr@(EAdd e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(ESub e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(EMul e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(EDiv e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(EIf0 c b1 b2) = do
      cTy <- typecheck (me, mt) c
      b1Ty <- typecheck (me, mt) b1
      b2Ty <- typecheck (me, mt) b2
      if cTy /= TInt
        then throwError ConditionalNotInt
      else if b1Ty /= b2Ty
        then throwError IfBranchNotSameType
        else return b1Ty
    typecheck' (me, mt) expr@(ELambda n t e) = do
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable n
        Nothing -> do
          eTy <- typecheck (Map.insert n t me, mt) e
          return $ TFunc t eTy
    typecheck' (me, mt) expr@(EApp f t) = do
      fTy <- typecheck (me, mt) f
      tTy <- typecheck (me, mt) t
      case fTy of
        TFunc tau sigma ->
          if tau /= tTy
            then throwError FunctionTypeMismatch
            else return sigma
        _ -> throwError NonFunctionApplication
    typecheck' (me, mt) expr@(ETupleGet e i) = do
      eTy <- typecheck (me, mt) e
      case eTy of
        TTuple ts ->
          if 0 <= i && length ts > i
            then return (ts !! i)
            else throwError BadTupleIndex
        _ -> throwError ExceptedTuple
    typecheck' (me, mt) expr@(ETuple es) = do
      TTuple <$> typecheck (me, mt) `mapM` es


typecheck0 = typecheck (Map.empty , Map.empty)