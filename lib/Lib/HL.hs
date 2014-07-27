{-# LANGUAGE DeriveGeneric #-}
module Lib.HL where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable (foldl', foldrM)

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lib.GCC as GCC hiding (Error)

import Debug.Trace

import Data.Hashable
import Data.Hashable.Generic ( gHashWithSalt )
import GHC.Generics

data Type = 
    TInt
  | TFunc Type Type
  | TList Type
  | TTuple [Type]
  | TVar String
  | TOther
  | TAny
  | TVariant [Type]
  deriving(Generic)

instance Eq Type where
  TAny == _                 = True
  _ == TAny                 = True
  TInt == TInt              = True
  TFunc a b == TFunc a' b'  = (a == a') && (b == b')
  TList a == TList a'       = a == a'
  TTuple l == TTuple l'     = l == l'
  TOther == TOther          = True
  TVariant l == TVariant l' = l == l'
  _ == _                    = False

instance Hashable Type where
  hashWithSalt s x = gHashWithSalt s x
  {-# INLINEABLE hashWithSalt #-}

type TypeHash = Int

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
  show (TVariant (t:ts))  = "<" ++ show t ++ (concat $ ((", " ++) . show) <$> ts)  ++ ">"
  show TOther           = "<OTHER>"
  show TAny             = "?"

data Expr =
    EConst TypeHash Type Int
  | EVar TypeHash Type String
  | ELet TypeHash Type String Expr Expr
  | ELetRec TypeHash Type String Type Expr Expr
  | EType TypeHash Type String Type Expr
  | EAdd TypeHash Type Expr Expr
  | ESub TypeHash Type Expr Expr
  | EMul TypeHash Type Expr Expr
  | EDiv TypeHash Type Expr Expr
  | EEq TypeHash Type Expr Expr
  | ENEq TypeHash Type Expr Expr
  | ELT TypeHash Type Expr Expr
  | ELTE TypeHash Type Expr Expr
  | EGT TypeHash Type Expr Expr
  | EGTE TypeHash Type Expr Expr
  | EIf TypeHash Type Expr Expr Expr
  | ELambda TypeHash Type String Type Expr
  | EApp TypeHash Type Expr Expr
  | ETuple TypeHash Type [Expr]
  | ETupleGet TypeHash Type Expr Int
  | EListFold TypeHash Type Expr Expr
  | EListCons TypeHash Type Expr Expr
  | EListEmpty TypeHash Type Type
  | EListIsEmpty TypeHash Type Expr
  | EListHead TypeHash Type Expr
  | EListTail TypeHash Type Expr
  | ENatFold TypeHash Type Expr Expr
  | EVariantConstruct TypeHash Type Type Int Expr
  | EVariantDestruct TypeHash Type [Expr]
  | ETrace TypeHash Type Expr Expr

instance Show Expr where
  show (EConst _ _ i)      = show i
  show (EVar _ _ s)        = s
  show (ELet _ _ n v e)    = "let " ++ n ++ " = " ++ show v ++ " in " ++ show e
  show (ELetRec _ _ n t v e)    = "letrec " ++ n ++ " : " ++ show t ++ " = " ++ show v ++ " in " ++ show e
  show (EType _ _ n v e)   = "type " ++ n ++ " = " ++ show v ++ " in " ++ show e
  show (EAdd _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " + " ++ "(" ++ show e2 ++ ")"
  show (ESub _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " - " ++ "(" ++ show e2 ++ ")"
  show (EMul _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " * " ++ "(" ++ show e2 ++ ")"
  show (EDiv _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " / " ++ "(" ++ show e2 ++ ")"
  show (EEq _ _ e1 e2)     = "(" ++ show e1 ++ ")" ++ " == " ++ "(" ++ show e2 ++ ")"
  show (ENEq _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " /= " ++ "(" ++ show e2 ++ ")"
  show (EGTE _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " >= " ++ "(" ++ show e2 ++ ")"
  show (EGT _ _ e1 e2)     = "(" ++ show e1 ++ ")" ++ " > " ++ "(" ++ show e2 ++ ")"
  show (ELT _ _ e1 e2)     = "(" ++ show e1 ++ ")" ++ " < " ++ "(" ++ show e2 ++ ")"
  show (ELTE _ _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " <= " ++ "(" ++ show e2 ++ ")"
  show (EIf _ _ c b1 b2)   = "if " ++ show c ++ " then " ++ show b1 ++ " else " ++ show b2
  show (ELambda _ _ n t e) = "\\" ++ n ++ " : " ++ show t ++ ". " ++ show e
  show (EApp _ _ f t)      = "(" ++ show f ++ ")" ++ show t
  show (ETupleGet _ _ e i) = "(" ++ show e ++ ")" ++ "[" ++ show i ++ "]"
  show (ETuple _ _ [])     = "()"
  show (ETuple _ _ (e:es)) = "(" ++ show e ++ (concat $ ((", " ++) . show) <$> es)  ++ ")"
  show (EListFold _ _ f x) = "fold " ++ "(" ++ show f ++ ")" ++ " with " ++ "(" ++ show x ++ ")"
  show (EListCons _ _ x xs) = "cons " ++ "(" ++ show x ++ ")" ++ " with " ++ "(" ++ show xs ++ ")"
  show (EListEmpty _ _ t) = "empty " ++ "(" ++ show t ++ ")"
  show (EListIsEmpty _ _ e) = "isempty " ++ "(" ++ show e ++ ")"
  show (EListHead _ _ t) = "head " ++ "(" ++ show t ++ ")"
  show (EListTail _ _ t) = "tail " ++ "(" ++ show t ++ ")"
  show (ENatFold _ _ f x) = "natfold " ++ "(" ++ show f ++ ")" ++ " with " ++ "(" ++ show x ++ ")"
  show (EVariantConstruct _ _ t i e) = "make " ++ "(" ++ show t ++ ")" ++ " " ++ show i ++ " " ++ "(" ++ show e ++ ")"
  show (EVariantDestruct _ _ (f:fs)) = "destruct " ++ "(" ++ show f ++ (concat $ ((", " ++) . show) <$> fs)  ++ ")"
  show (ETrace _ _ a b) = "trace " ++ "(" ++ show a ++ ")" ++ " in " ++ "(" ++ show b ++ ")"

exprType :: Expr -> Type
exprType (EConst _ ty i)      = ty
exprType (EVar _ ty s)        = ty
exprType (ELet _ ty n v e)    = ty
exprType (ELetRec _ ty n t v e)    = ty
exprType (EType _ ty n v e)   = ty
exprType (EAdd _ ty e1 e2)    = ty
exprType (ESub _ ty e1 e2)    = ty
exprType (EMul _ ty e1 e2)    = ty
exprType (EDiv _ ty e1 e2)    = ty
exprType (EEq _ ty e1 e2)     = ty
exprType (ENEq _ ty e1 e2)    = ty
exprType (EGTE _ ty e1 e2)    = ty
exprType (EGT _ ty e1 e2)     = ty
exprType (ELT _ ty e1 e2)     = ty
exprType (ELTE _ ty e1 e2)    = ty
exprType (EIf _ ty c b1 b2)   = ty
exprType (ELambda _ ty n t e) = ty
exprType (EApp _ ty f t)      = ty
exprType (ETupleGet _ ty e i) = ty
exprType (ETuple _ ty [])     = ty
exprType (ETuple _ ty (e:es)) = ty
exprType (EListFold _ ty _ _) = ty
exprType (EListCons _ ty _ _) = ty
exprType (EListEmpty _ ty _) = ty
exprType (EListIsEmpty _ ty _) = ty
exprType (EListHead _ ty _) = ty
exprType (EListTail _ ty _) = ty
exprType (ENatFold _ ty _ _) = ty
exprType (EVariantConstruct _ ty _ _ _) = ty
exprType (EVariantDestruct _ ty _) = ty
exprType (ETrace _ ty _ _) = ty

exprTypeHash :: Expr -> TypeHash
exprTypeHash (EConst ty _ i)      = ty
exprTypeHash (EVar ty _ s)        = ty
exprTypeHash (ELet ty _ n v e)    = ty
exprTypeHash (ELetRec ty _ n t v e)    = ty
exprTypeHash (EType ty _ n v e)   = ty
exprTypeHash (EAdd ty _ e1 e2)    = ty
exprTypeHash (ESub ty _ e1 e2)    = ty
exprTypeHash (EMul ty _ e1 e2)    = ty
exprTypeHash (EDiv ty _ e1 e2)    = ty
exprTypeHash (EEq ty _ e1 e2)     = ty
exprTypeHash (ENEq ty _ e1 e2)    = ty
exprTypeHash (EGTE ty _ e1 e2)    = ty
exprTypeHash (EGT ty _ e1 e2)     = ty
exprTypeHash (ELT ty _ e1 e2)     = ty
exprTypeHash (ELTE ty _ e1 e2)    = ty
exprTypeHash (EIf ty _ c b1 b2)   = ty
exprTypeHash (ELambda ty _ n t e) = ty
exprTypeHash (EApp ty _ f t)      = ty
exprTypeHash (ETupleGet ty _ e i) = ty
exprTypeHash (ETuple ty _ [])     = ty
exprTypeHash (ETuple ty _ (e:es)) = ty
exprTypeHash (EListFold ty _ _ _) = ty
exprTypeHash (EListCons ty _ _ _) = ty
exprTypeHash (EListEmpty ty _ _) = ty
exprTypeHash (EListIsEmpty ty _ _) = ty
exprTypeHash (EListHead ty _ _) = ty
exprTypeHash (EListTail ty _ _) = ty
exprTypeHash (ENatFold ty _ _ _) = ty
exprTypeHash (EVariantConstruct ty _ _ _ _) = ty
exprTypeHash (EVariantDestruct ty _ _) = ty
exprTypeHash (ETrace ty _ _ _) = ty

withType :: (Type -> Type) -> Expr -> Expr
withType fty (EConst _ ty i)      = let nty = (fty ty) in EConst (hash nty) nty i
withType fty (EVar _ ty s)        = let nty = (fty ty) in EVar (hash nty) nty s
withType fty (ELet _ ty n v e)    = let nty = (fty ty) in ELet (hash nty) nty n v e
withType fty (ELetRec _ ty n t v e)    = let nty = (fty ty) in ELetRec (hash nty) nty n t v e
withType fty (EType _ ty n v e)   = let nty = (fty ty) in EType (hash nty) nty n v e
withType fty (EAdd _ ty e1 e2)    = let nty = (fty ty) in EAdd (hash nty) nty e1 e2
withType fty (ESub _ ty e1 e2)    = let nty = (fty ty) in ESub (hash nty) nty e1 e2
withType fty (EMul _ ty e1 e2)    = let nty = (fty ty) in EMul (hash nty) nty e1 e2
withType fty (EDiv _ ty e1 e2)    = let nty = (fty ty) in EDiv (hash nty) nty e1 e2
withType fty (EEq _ ty e1 e2)     = let nty = (fty ty) in EEq (hash nty) nty e1 e2
withType fty (ENEq _ ty e1 e2)    = let nty = (fty ty) in ENEq (hash nty) nty e1 e2
withType fty (EGTE _ ty e1 e2)    = let nty = (fty ty) in EGTE (hash nty) nty e1 e2
withType fty (EGT _ ty e1 e2)     = let nty = (fty ty) in EGT (hash nty) nty e1 e2
withType fty (ELT _ ty e1 e2)     = let nty = (fty ty) in ELT (hash nty) nty e1 e2
withType fty (ELTE _ ty e1 e2)    = let nty = (fty ty) in ELTE (hash nty) nty e1 e2
withType fty (EIf _ ty c b1 b2)   = let nty = (fty ty) in EIf (hash nty) nty c b1 b2
withType fty (ELambda _ ty n t e) = let nty = (fty ty) in ELambda (hash nty) nty n t e
withType fty (EApp _ ty f t)      = let nty = (fty ty) in EApp (hash nty) nty f t
withType fty (ETupleGet _ ty e i) = let nty = (fty ty) in ETupleGet (hash nty) nty e i
withType fty (ETuple _ ty [])     = let nty = (fty ty) in ETuple (hash nty) nty []
withType fty (ETuple _ ty (e:es)) = let nty = (fty ty) in ETuple (hash nty) nty (e:es)
withType fty (EListFold _ ty f x) = let nty = (fty ty) in EListFold (hash nty) nty f x
withType fty (EListCons _ ty x xs) = let nty = (fty ty) in EListCons (hash nty) nty x xs
withType fty (EListEmpty _ ty t) = let nty = (fty ty) in EListEmpty (hash nty) nty t
withType fty (EListIsEmpty _ ty t) = let nty = (fty ty) in EListIsEmpty (hash nty) nty t
withType fty (EListHead _ ty l) = let nty = (fty ty) in EListHead (hash nty) nty l
withType fty (EListTail _ ty l) = let nty = (fty ty) in EListTail (hash nty) nty l
withType fty (ENatFold _ ty f x) = let nty = (fty ty) in ENatFold (hash nty) nty f x
withType fty (EVariantConstruct _ ty a b c) = let nty = (fty ty) in EVariantConstruct (hash nty) nty a b c
withType fty (EVariantDestruct _ ty l) = let nty = (fty ty) in EVariantDestruct (hash nty) nty l
withType fty (ETrace _ ty a b) = let nty = (fty ty) in ETrace (hash nty) nty a b

data Error =
    DuplicateVariable Expr String
  | UnknownVariable Expr String
  | ArithNotInt Expr
  | ConditionalNotInt Expr
  | IfBranchNotSameType Expr
  | NonFunctionApplication Expr
  | FunctionTypeMismatch Expr
  | BadTupleIndex Expr
  | ExceptedTuple Expr
  | ExceptedList Expr
  | ExceptedVariant Expr
  | BadVariantType Expr
  | BadLetRecType Expr Type Type
  | UnknownTypeVariable Expr String
  | UnknownTypeVariableSubst String
  deriving(Show)

substituteType :: Map String Type -> Type -> Either Error Type
substituteType mp (TInt) = return TInt
substituteType mp (TVar s) = case Map.lookup s mp of
  Nothing -> throwError $ UnknownTypeVariableSubst s
  Just x -> return x
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
substituteType mp TAny = return TAny
substituteType mp (TVariant ts) =
  TVariant <$> foldrM (\t ts' -> do
      t' <- substituteType mp t
      return $ t':ts'
    ) [] ts

typecheck :: (Map String Type, Map String Type) -> Expr -> Either Error Expr
typecheck (me, mt) e = do
  e' <- typecheck' (me, mt) e
  eTy <- substituteType mt (exprType e')
  return $ withType (\_ -> eTy) e'
  where
    typecheck' :: (Map String Type, Map String Type) -> Expr -> Either Error Expr
    typecheck' (me, mt) expr@(EConst _ _ i)     = return $ EConst 0 TInt i
    typecheck' (me, mt) expr@(EVar _ _ s)       = case Map.lookup s me of
      Nothing -> throwError $ UnknownVariable expr s
      Just x  -> return $ EVar 0 x s
    typecheck' (me, mt) expr@(ELet _ _ n v e)   =
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          v' <- typecheck (me, mt) v
          e' <- typecheck (Map.insert n (exprType v') me, mt) e
          return $ ELet 0 (exprType e') n v' e'
    typecheck' (me, mt) expr@(ELetRec _ _ n t v e)   = do
      t' <- substituteType mt t
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          v' <- typecheck (Map.insert n t' me, mt) v
          if exprTypeHash v' == hash t' || exprType v' == t'
            then do
              e' <- typecheck (Map.insert n t' me, mt) e
              return $ ELetRec 0 (exprType e') n t' v' e'
            else throwError $ BadLetRecType expr (exprType v') t'
    typecheck' (me, mt) expr@(EType _ _ n v e)   = do
      v' <- substituteType mt v
      case Map.lookup n mt of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          e' <- typecheck (me, Map.insert n v' mt) e
          return $ EType 0 (exprType e') n v e'
    typecheck' (me, mt) expr@(EAdd _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ EAdd 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ESub _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ ESub 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EMul _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ EMul 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EDiv _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ EDiv 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EEq _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ EEq 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ENEq _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ ENEq 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EGT _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ EGT 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EGTE _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ EGTE 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ELTE _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ ELTE 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ELT _ _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprTypeHash e1' == hash TInt && exprTypeHash e2' == hash TInt
        then return $ ELT 0 TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EIf _ _ c b1 b2) = do
      c' <- typecheck (me, mt) c
      b1' <- typecheck (me, mt) b1
      b2' <- typecheck (me, mt) b2
      if exprTypeHash c' /= hash TInt
        then throwError $ ConditionalNotInt expr
      else if exprTypeHash b1' /= exprTypeHash b2'
        then throwError $ IfBranchNotSameType expr
        else return $ EIf 0 (exprType b1') c' b1' b2'
    typecheck' (me, mt) expr@(ELambda _ _ n t e) = do
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          t' <- substituteType mt t
          e' <- typecheck (Map.insert n t' me, mt) e
          return $ ELambda 0 (TFunc t $ exprType e') n t' e'
    typecheck' (me, mt) expr@(EApp _ _ f t) = do
      f' <- typecheck (me, mt) f
      t' <- typecheck (me, mt) t
      case exprType f' of
        TFunc tau sigma ->
          if hash tau /= exprTypeHash t' && tau /= exprType t'
            then traceShow (tau, exprType t') $ throwError $ FunctionTypeMismatch expr
            else return $ EApp 0 sigma f' t'
        _ -> throwError $ NonFunctionApplication expr
    typecheck' (me, mt) expr@(ETupleGet _ _ e i) = do
      e' <- typecheck (me, mt) e
      case exprType e' of
        TTuple ts ->
          if 0 <= i && length ts > i
            then return $ ETupleGet 0 (ts !! i) e' i
            else throwError $ BadTupleIndex expr
        _ -> throwError $ ExceptedTuple expr
    typecheck' (me, mt) expr@(ETuple _ _ es) = do
      es' <- typecheck (me, mt) `mapM` es
      return $ ETuple 0 (TTuple $ exprType <$> es') es'
    typecheck' (me, mt) expr@(EListFold _ _ f x) = do
      f' <- typecheck (me, mt) f
      x' <- typecheck (me, mt) x
      case exprType f' of
        TFunc a (TFunc sigma sigma') -> 
          if hash sigma == hash sigma' && hash sigma == exprTypeHash x'
            then return $ EListFold 0 (TFunc (TList a) sigma) f' x'
            else traceShow (sigma, sigma', exprType x') $ throwError $ FunctionTypeMismatch expr
        _ -> throwError $ NonFunctionApplication expr
    typecheck' (me, mt) expr@(EListCons _ _ x xs) = do
      x' <- typecheck (me, mt) x
      xs' <- typecheck (me, mt) xs
      if exprTypeHash xs' == hash (TList (exprType x'))
        then return $ EListCons 0 (TList (exprType x')) x' xs'
        else throwError $ FunctionTypeMismatch expr
    typecheck' (me, mt) expr@(EListEmpty _ _ t) = do
      return $ EListEmpty 0 (TList t) t
    typecheck' (me, mt) expr@(EListIsEmpty _ _ e) = do
      e' <- typecheck (me, mt) e
      case exprType e' of
        TList a -> return $ EListIsEmpty 0 TInt e'
        _ -> throwError $ ExceptedList expr
    typecheck' (me, mt) expr@(EListHead _ _ l) = do
      l' <- typecheck (me, mt) l
      case exprType l' of
        TList a -> return $ EListHead 0 a l'
        _ -> throwError $ ExceptedList expr
    typecheck' (me, mt) expr@(EListTail _ _ l) = do
      l' <- typecheck (me, mt) l
      case exprType l' of
        TList a -> return $ EListTail 0 (TList a) l'
        _ -> throwError $ ExceptedList expr
    typecheck' (me, mt) expr@(ENatFold _ _ f x) = do
      f' <- typecheck (me, mt) f
      x' <- typecheck (me, mt) x
      case exprType f' of
        TFunc TInt (TFunc sigma sigma') -> 
          if hash sigma == hash sigma' && hash sigma == exprTypeHash x'
            then return $ ENatFold 0 (TFunc TInt sigma) f' x'
            else traceShow (sigma, sigma', exprType x') $ throwError $ FunctionTypeMismatch expr
        _ -> throwError $ NonFunctionApplication expr
    typecheck' (me, mt) expr@(EVariantConstruct _ _ ty i e) = do
      ty' <- substituteType mt ty
      case ty' of
        TVariant ts ->
          if 0 <= i && i < length ts
            then do
              e' <- typecheck (me, mt) e
              if (exprTypeHash e' == hash (ts !! i)) || (exprType e' == ts !! i)
                then return $ EVariantConstruct 0 ty' ty' i e'
                else traceShow (exprType e', ts !! i) $ throwError $ BadVariantType expr
            else throwError $ BadVariantType expr
        _ -> throwError $ ExceptedVariant expr
    typecheck' (me, mt) expr@(EVariantDestruct _ _ fs) = do
      fs' <- typecheck (me, mt) `mapM` fs
      l <- forM (exprType <$> fs') $ \ty ->
        case ty of
          TFunc tau sigma -> return (tau, sigma)
          _ -> throwError $ NonFunctionApplication expr
      let ts = fst <$> l 
      let ss = snd <$> l 
      if all (== (hash $ ss !! 0)) (hash <$> ss)
        then return $ EVariantDestruct 0 (TFunc (TVariant ts) (ss !! 0)) fs'
        else throwError $ NonFunctionApplication expr
    typecheck' (me, mt) expr@(ETrace _ _ a b) = do
      a' <- typecheck (me, mt) a
      b' <- typecheck (me, mt) b
      return $ ETrace 0 (exprType b') a' b'


typecheck0 = typecheck (Map.empty, Map.empty)

-- Compilation

data VariableLocation =
    VLEnvironment Int Int
  | VLGlobal String
  | VLConstant Int

data CompilationState = CompilationState
  { currentLabelName :: String
  , variableLocation :: Map String VariableLocation
  }


type CMonad a = ReaderT CompilationState (StateT Int (Writer [Instruction])) a

blockBegin :: String -> CMonad String
blockBegin n = do
  state <- ask
  return $ currentLabelName state ++ "." ++ n ++ ".BEGIN"

blockEnd :: String -> CMonad String
blockEnd n = do
  state <- ask
  return $ currentLabelName state ++ "." ++ n ++ ".END"

block :: String -> CMonad () -> CMonad ()
block n m = do
  bbegin <- blockBegin n
  bend <- blockEnd n
  tell [LABEL bbegin]
  local (\state -> state {currentLabelName = currentLabelName state ++ "." ++ n}) m
  tell [LABEL bend]

increaseEnvironment :: VariableLocation -> VariableLocation
increaseEnvironment (VLEnvironment n i) = VLEnvironment (n+1) i
increaseEnvironment v = v

bindVariable :: String -> CMonad () -> CMonad ()
bindVariable s = local (\state -> state {variableLocation = Map.insert s (VLEnvironment 0 0) $ Map.map increaseEnvironment (variableLocation state)})

increaseState :: CMonad ()
increaseState = modify (+ 1)

runHL :: CMonad () -> [Instruction]
runHL m = execWriter $ runStateT (runReaderT m $ CompilationState "." Map.empty) 0

compile :: Expr -> CMonad ()
compile (EConst _ _ i)          = tell [LDC $ VInt i]
compile (EVar _ _ s)            = do
  state <- ask
  case Map.lookup s (variableLocation state) of
    Just (VLEnvironment n i) -> tell [LD (VInt n) (VInt i)]
    Just (VLGlobal lbl)      -> tell [LDC (VLabel lbl)]
    Just (VLConstant i)      -> tell [LDC (VInt i)]
    Nothing -> error $ "UnknownVariable " ++ s
compile (ELet _ _ n v e)        = compile (EApp (hash $ exprType e) (exprType e) (ELambda (hash (TFunc (exprType v) (exprType e))) (TFunc (exprType v) (exprType e)) n (exprType v) e) v)
compile (ELetRec _ _ n t v e)   = do
  id <- get
  increaseState
  let blockName = "letrec." ++ show id
  bbegin <- blockBegin blockName
  bend <- blockEnd blockName
  
  id <- get
  increaseState
  let lambdaBlock = "lambda." ++ show id
  lambdaBegin <- blockBegin lambdaBlock
  lambdaEnd <- blockEnd lambdaBlock

  id <- get
  increaseState
  let lambda2Block = "lambda2." ++ show id
  lambda2Begin <- blockBegin lambda2Block
  lambda2End <- blockEnd lambda2Block

  tell [LDC (VInt 0), TSEL (VLabel lambda2End) (VLabel lambda2End)]
  block blockName $ bindVariable "?" $ bindVariable n $ bindVariable "?" $ do
    tell [LD (VInt 0) (VInt 0)]
    compile v
    tell [AP (VInt 1), RTN]
  block lambdaBlock $ do
    tell
      [ LD (VInt 1) (VInt 0)
      , LD (VInt 0) (VInt 0)
      , AP (VInt 1)
      , RTN
      ]
  block lambda2Block $ do
    tell
      [ DUM (VInt 1)
      , LDF (VLabel bbegin)
      , LDF (VLabel lambdaBegin)
      , RAP (VInt 1)
      , RTN
      ]
  tell [LDF (VLabel lambda2Begin)]
  compile $ ELambda (hash (TFunc (exprType v) (exprType e))) (TFunc (exprType v) (exprType e)) n (exprType v) e
  tell [AP $ VInt 1]
compile (EType _ _ n v e)       = compile e
compile (EAdd _ _ e1 e2)        = do
  compile e1
  compile e2
  tell [ADD]
compile (ESub _ _ e1 e2)        = do
  compile e1
  compile e2
  tell [SUB]
compile (EMul _ _ e1 e2)        = do
  compile e1
  compile e2
  tell [MUL]
compile (EDiv _ _ e1 e2)        = do
  compile e1
  compile e2
  tell [DIV]
compile (EEq _ _ e1 e2)         = do
  compile e1
  compile e2
  tell [CEQ]
compile (ENEq _ _ e1 e2)        = compile $ EApp (hash TInt) TInt (EVar (hash (TFunc TInt TInt)) (TFunc TInt TInt) "not") $ EEq (hash TInt) TInt e1 e2
compile (EGTE _ _ e1 e2)        = do
  compile e1
  compile e2
  tell [CGTE]
compile (EGT _ _ e1 e2)         = do
  compile e1
  compile e2
  tell [CGT]
compile (ELT _ _ e1 e2)         = compile $ EApp (hash TInt) TInt (EVar (hash (TFunc TInt TInt)) (TFunc TInt TInt) "not") $ EGTE (hash TInt) TInt e1 e2
compile (ELTE _ _ e1 e2)        = compile $ EApp (hash TInt) TInt (EVar (hash (TFunc TInt TInt)) (TFunc TInt TInt) "not") $ EGT (hash TInt) TInt e1 e2
compile (EIf _ _ c b1 b2)      = do
  id <- get
  increaseState
  let blockName = "if." ++ show id
  thenbegin <- blockBegin $ blockName ++ ".then"
  elsebegin <- blockBegin $ blockName ++ ".else"
  bend <- blockEnd blockName
  tell [LDC $ VInt 0, TSEL (VLabel bend) (VLabel bend)]
  block blockName $ do
    block "then" $ do
      compile b1
      tell [JOIN]
    block "else" $ do
      compile b2
      tell [JOIN]
  compile c
  tell [SEL (VLabel thenbegin) (VLabel elsebegin)]
compile (ELambda _ _ n t e)     = do
  id <- get
  increaseState
  let blockName = "lambda." ++ show id
  bbegin <- blockBegin blockName
  bend <- blockEnd blockName
  tell [LDC $ VInt 0, TSEL (VLabel bend) (VLabel bend)]
  block blockName $ bindVariable n $ do
    compile e
    tell [RTN]
  tell [LDF $ VLabel bbegin]
compile (EApp _ _ f t)          = do
  compile t -- t is on stack
  compile f -- f closure is on stack
  tell [AP $ VInt 1]
compile (ETupleGet _ _ e i)    = do
  compile e
  case exprType e of
    TTuple ts -> compileTupleGet i ts
    err -> traceShow (e, err) $ error "error"
  where
    compileTupleGet 0 (t:[]) = return ()
    compileTupleGet 0 (t:ts) = tell [CAR]
    compileTupleGet i (t:ts) = do
      tell [CDR]
      compileTupleGet (i-1) ts
compile (ETuple _ _ [])         = error "unimplemented ETuple (empty)"
compile (ETuple _ _ (e:es))     = compileTuple (e:es)
  where
    compileTuple (e1:e2:[]) = do
      compile e1
      compile e2
      tell [CONS]
    compileTuple (e:es)     = do
      compile e
      compileTuple es
      tell [CONS]
compile (EListFold _ _ f x)         = do
  id <- get
  increaseState
  let blockName = "fold." ++ show id
  bbegin <- blockBegin blockName
  bend <- blockEnd blockName
  
  id' <- get
  increaseState
  let lambdaBlock = "lambda." ++ show id'
  lambdaBegin <- blockBegin lambdaBlock
  lambdaEnd <- blockEnd lambdaBlock
  
  id' <- get
  increaseState
  let lambda2Block = "lambda2." ++ show id'
  lambda2Begin <- blockBegin lambda2Block
  lambda2End <- blockEnd lambda2Block

  tell [LDC $ VInt 0, TSEL (VLabel lambda2End) (VLabel lambda2End)]
  block blockName $ bindVariable "?" $ bindVariable "?" $ bindVariable "?" $ do
    thenbegin <- blockBegin "then"
    elsebegin <- blockBegin "else"
    tell [LD (VInt 0) (VInt 0), CDR, ATOM, TSEL (VLabel thenbegin) (VLabel elsebegin)]
    block "then" $ tell
      [ LD (VInt 0) (VInt 0)
      , CAR
      , RTN
      ]
    block "else" $ do
      tell
        [ LD (VInt 0) (VInt 0)
        , CAR
        , LD (VInt 0) (VInt 0)
        , CDR
        , CAR
        ]
      compile f
      tell
        [ AP (VInt 1)
        , AP (VInt 1)
        , LD (VInt 0) (VInt 0)
        , CDR
        , CDR
        , CONS
        , LD (VInt 1) (VInt 0)
        , TAP (VInt 1)
        ]
  block lambdaBlock $ bindVariable "?" $ bindVariable "?" $ do
    compile x
    tell
      [ LD (VInt 1) (VInt 0)
      , CONS
      , LD (VInt 0) (VInt 0)
      , AP (VInt 1)
      , RTN
      ]
  block lambda2Block $ bindVariable "?" $ do
    tell
      [ DUM (VInt 1)
      , LDF (VLabel bbegin)
      , LDF (VLabel lambdaBegin)
      , RAP (VInt 1)
      , RTN
      ]
  tell [LDF (VLabel lambda2Begin)]
compile (EListCons _ _ x xs)         = do
  compile x
  compile xs
  tell [CONS]
compile (EListEmpty _ _ _)           = tell [LDC (VInt 0)]
compile (EListIsEmpty _ _ e)           = do
  compile e
  tell [ATOM]
compile (EListHead _ _ l)           = do
  compile l
  tell [CAR]
compile (EListTail _ _ l)           = do
  compile l
  tell [CDR]
compile (ENatFold _ _ f x)         = do
  id <- get
  increaseState
  let blockName = "fold." ++ show id
  bbegin <- blockBegin blockName
  bend <- blockEnd blockName
  
  id' <- get
  increaseState
  let lambdaBlock = "lambda." ++ show id'
  lambdaBegin <- blockBegin lambdaBlock
  lambdaEnd <- blockEnd lambdaBlock
  
  id' <- get
  increaseState
  let lambda2Block = "lambda2." ++ show id'
  lambda2Begin <- blockBegin lambda2Block
  lambda2End <- blockEnd lambda2Block

  tell [LDC $ VInt 0, TSEL (VLabel lambda2End) (VLabel lambda2End)]
  block blockName $ bindVariable "?" $ bindVariable "?" $ bindVariable "?" $ do
    thenbegin <- blockBegin "then"
    elsebegin <- blockBegin "else"
    tell [LD (VInt 0) (VInt 0), CDR, TSEL (VLabel thenbegin) (VLabel elsebegin)]
    block "then" $ do
      tell
        [ LD (VInt 0) (VInt 0)
        , CAR
        , LD (VInt 0) (VInt 0)
        , CDR
        ]
      compile f
      tell
        [ AP (VInt 1)
        , AP (VInt 1)
        , LD (VInt 0) (VInt 0)
        , CDR
        , LDC (VInt 1)
        , SUB
        , CONS
        , LD (VInt 1) (VInt 0)
        , TAP (VInt 1)
        ]
    block "else" $ tell
      [ LD (VInt 0) (VInt 0)
      , CAR
      , RTN
      ]
  block lambdaBlock $ bindVariable "?" $ bindVariable "?" $ do
    compile x
    tell
      [ LD (VInt 1) (VInt 0)
      , CONS
      , LD (VInt 0) (VInt 0)
      , AP (VInt 1)
      , RTN
      ]
  block lambda2Block $ bindVariable "?" $ do
    tell
      [ DUM (VInt 1)
      , LDF (VLabel bbegin)
      , LDF (VLabel lambdaBegin)
      , RAP (VInt 1)
      , RTN
      ]
  tell [LDF (VLabel lambda2Begin)]
compile (EVariantConstruct _ _ ty i e) = do
  tell [LDC (VInt i)]
  compile e
  tell [CONS]
compile (EVariantDestruct _ _ fs) = do
  let n = length fs
  id <- get
  increaseState
  let blockName = "destruct." ++ show id
  bbegin <- blockBegin blockName
  bend <- blockEnd blockName

  tell [LDC $ VInt 0, TSEL (VLabel bend) (VLabel bend)]
  block blockName $ bindVariable "?" $ do
    tell [LD (VInt 0) (VInt 0), CDR]
    compileVariantDestruct 0 (n-1)
  tell [LDF $ VLabel bbegin]
    where
      compileVariantDestruct i j
        | i == j    = do
            compile (fs !! i)
            tell [AP (VInt 1), RTN]
        | otherwise = do
            let k = (i+j)`div`2
            id <- get
            increaseState
            let ifBlock = "if." ++ show id
            ifEnd <- blockEnd ifBlock
            thenbegin <- blockBegin $ "if." ++ show id ++ ".then"
            elsebegin <- blockBegin $ "if." ++ show id ++ ".else"
            tell [LDC (VInt 0), TSEL (VLabel ifEnd) (VLabel ifEnd)]
            block ifBlock $ do
              block "then" $ compileVariantDestruct (k+1) j
              block "else" $ compileVariantDestruct i k
            tell [LD (VInt 0) (VInt 0), CAR, LDC (VInt k), CGT, TSEL (VLabel thenbegin) (VLabel elsebegin)]
compile (ETrace _ _ a b) = do
  compile a
  tell [DBUG]
  compile b


fullCompile :: Expr -> CMonad ()
fullCompile e = do
  compile e
  tell [RTN]

toPacman :: Expr -> CMonad ()
toPacman e = do
  tell
    [ LD (VInt 0) (VInt 1)
    , LD (VInt 0) (VInt 0)
    ]
  compile e -- Main is on top stack
  id <- get
  increaseState
  let mainBlock = "MAIN." ++ show id
  let stepBlock = "STEP." ++ show id
  mainBlockBegin <- blockBegin mainBlock
  stepBlockBegin <- blockBegin stepBlock
  tell
    [ AP (VInt 1)
    , AP (VInt 1)
    , LDF (VLabel $ mainBlockBegin)
    , AP (VInt 1)
    , RTN
    ]
  block mainBlock $ tell
    [ LD (VInt 0) (VInt 0)
    , CAR
    , LDF (VLabel $ stepBlockBegin)
    , CONS
    , RTN
    ]
  block stepBlock $ tell
    [ LD (VInt 0) (VInt 1)
    , LD (VInt 0) (VInt 0)
    , LD (VInt 1) (VInt 0)
    , CDR
    , AP (VInt 1)
    , AP (VInt 1)
    , RTN
    ]