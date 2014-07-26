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

data Expr =
    EConst Type Int
  | EVar Type String
  | ELet Type String Expr Expr
  | EType Type String Type Expr
  | EAdd Type Expr Expr
  | ESub Type Expr Expr
  | EMul Type Expr Expr
  | EDiv Type Expr Expr
  | EEq Type Expr Expr
  | ENEq Type Expr Expr
  | ELT Type Expr Expr
  | ELTE Type Expr Expr
  | EGT Type Expr Expr
  | EGTE Type Expr Expr
  | EIf Type Expr Expr Expr
  | ELambda Type String Type Expr
  | EApp Type Expr Expr
  | ETuple Type [Expr]
  | ETupleGet Type Expr Int
  | EListFold Type Expr Expr
  | EListCons Type Expr Expr
  | EListEmpty Type Type

instance Show Expr where
  show (EConst _ i)      = show i
  show (EVar _ s)        = s
  show (ELet _ n v e)    = "let " ++ n ++ " = " ++ show v ++ " in " ++ show e
  show (EType _ n v e)   = "let " ++ n ++ " = " ++ show v ++ " in " ++ show e
  show (EAdd _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " + " ++ "(" ++ show e2 ++ ")"
  show (ESub _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " - " ++ "(" ++ show e2 ++ ")"
  show (EMul _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " * " ++ "(" ++ show e2 ++ ")"
  show (EDiv _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " / " ++ "(" ++ show e2 ++ ")"
  show (EEq _ e1 e2)     = "(" ++ show e1 ++ ")" ++ " == " ++ "(" ++ show e2 ++ ")"
  show (ENEq _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " /= " ++ "(" ++ show e2 ++ ")"
  show (EGTE _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " >= " ++ "(" ++ show e2 ++ ")"
  show (EGT _ e1 e2)     = "(" ++ show e1 ++ ")" ++ " > " ++ "(" ++ show e2 ++ ")"
  show (ELT _ e1 e2)     = "(" ++ show e1 ++ ")" ++ " < " ++ "(" ++ show e2 ++ ")"
  show (ELTE _ e1 e2)    = "(" ++ show e1 ++ ")" ++ " <= " ++ "(" ++ show e2 ++ ")"
  show (EIf _ c b1 b2)   = "if " ++ show c ++ " then " ++ show b1 ++ " else " ++ show b2
  show (ELambda _ n t e) = "\\" ++ n ++ " : " ++ show t ++ ". " ++ show e
  show (EApp _ f t)      = "(" ++ show f ++ ")" ++ show t
  show (ETupleGet _ e i) = "(" ++ show e ++ ")" ++ "[" ++ show i ++ "]"
  show (ETuple _ [])     = "()"
  show (ETuple _ (e:es)) = "(" ++ show e ++ (concat $ ((", " ++) . show) <$> es)  ++ ")"
  show (EListFold _ f x) = "fold " ++ "(" ++ show f ++ ")" ++ " with " ++ "(" ++ show x ++ ")"
  show (EListCons _ x xs) = "cons " ++ "(" ++ show x ++ ")" ++ " with " ++ "(" ++ show xs ++ ")"
  show (EListEmpty _ t) = "empty " ++ "(" ++ show t ++ ")"

exprType :: Expr -> Type
exprType (EConst ty i)      = ty
exprType (EVar ty s)        = ty
exprType (ELet ty n v e)    = ty
exprType (EType ty n v e)   = ty
exprType (EAdd ty e1 e2)    = ty
exprType (ESub ty e1 e2)    = ty
exprType (EMul ty e1 e2)    = ty
exprType (EDiv ty e1 e2)    = ty
exprType (EEq ty e1 e2)     = ty
exprType (ENEq ty e1 e2)    = ty
exprType (EGTE ty e1 e2)    = ty
exprType (EGT ty e1 e2)     = ty
exprType (ELT ty e1 e2)     = ty
exprType (ELTE ty e1 e2)    = ty
exprType (EIf ty c b1 b2)   = ty
exprType (ELambda ty n t e) = ty
exprType (EApp ty f t)      = ty
exprType (ETupleGet ty e i) = ty
exprType (ETuple ty [])     = ty
exprType (ETuple ty (e:es)) = ty
exprType (EListFold ty _ _) = ty
exprType (EListCons ty _ _) = ty
exprType (EListEmpty ty _) = ty

withType :: (Type -> Type) -> Expr -> Expr
withType fty (EConst ty i)      = EConst (fty ty) i
withType fty (EVar ty s)        = EVar (fty ty) s
withType fty (ELet ty n v e)    = ELet (fty ty) n v e
withType fty (EType ty n v e)   = EType (fty ty) n v e
withType fty (EAdd ty e1 e2)    = EAdd (fty ty) e1 e2
withType fty (ESub ty e1 e2)    = ESub (fty ty) e1 e2
withType fty (EMul ty e1 e2)    = EMul (fty ty) e1 e2
withType fty (EDiv ty e1 e2)    = EDiv (fty ty) e1 e2
withType fty (EEq ty e1 e2)     = EEq (fty ty) e1 e2
withType fty (ENEq ty e1 e2)    = ENEq (fty ty) e1 e2
withType fty (EGTE ty e1 e2)    = EGTE (fty ty) e1 e2
withType fty (EGT ty e1 e2)     = EGT (fty ty) e1 e2
withType fty (ELT ty e1 e2)     = ELT (fty ty) e1 e2
withType fty (ELTE ty e1 e2)    = ELTE (fty ty) e1 e2
withType fty (EIf ty c b1 b2)   = EIf (fty ty) c b1 b2
withType fty (ELambda ty n t e) = ELambda (fty ty) n t e
withType fty (EApp ty f t)      = EApp (fty ty) f t
withType fty (ETupleGet ty e i) = ETupleGet (fty ty) e i
withType fty (ETuple ty [])     = ETuple (fty ty) []
withType fty (ETuple ty (e:es)) = ETuple (fty ty) (e:es)
withType fty (EListFold ty f x) = EListFold (fty ty) f x
withType fty (EListCons ty x xs) = EListCons (fty ty) x xs
withType fty (EListEmpty ty t) = EListEmpty (fty ty) t

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
  | UnknownTypeVariable Expr String
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

typecheck :: (Map String Type, Map String Type) -> Expr -> Either Error Expr
typecheck (me, mt) e = do
  e' <- typecheck' (me, mt) e
  eTy <- substituteType mt (exprType e')
  return $ withType (\_ -> eTy) e'
  where
    typecheck' :: (Map String Type, Map String Type) -> Expr -> Either Error Expr
    typecheck' (me, mt) expr@(EConst _ i)     = return $ EConst TInt i
    typecheck' (me, mt) expr@(EVar _ s)       = case Map.lookup s me of
      Nothing -> throwError $ UnknownVariable expr s
      Just x  -> return $ EVar x s
    typecheck' (me, mt) expr@(ELet _ n v e)   =
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          v' <- typecheck (me, mt) v
          e' <- typecheck (Map.insert n (exprType v') me, mt) e
          return $ ELet (exprType e') n v' e'
    typecheck' (me, mt) expr@(EType _ n v e)   = do
      v' <- substituteType mt v
      case Map.lookup n mt of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          e' <- typecheck (me, Map.insert n v' mt) e
          return $ EType (exprType e') n v e'
    typecheck' (me, mt) expr@(EAdd _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ EAdd TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ESub _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ ESub TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EMul _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ EMul TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EDiv _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ EDiv TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EEq _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ EEq TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ENEq _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ ENEq TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EGT _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ EGT TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EGTE _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ EGTE TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ELTE _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ ELTE TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(ELT _ e1 e2)   = do
      e1' <- typecheck (me, mt) e1
      e2' <- typecheck (me, mt) e2
      if exprType e1' == TInt && exprType e2' == TInt
        then return $ ELT TInt e1' e2'
        else throwError $ ArithNotInt expr
    typecheck' (me, mt) expr@(EIf _ c b1 b2) = do
      c' <- typecheck (me, mt) c
      b1' <- typecheck (me, mt) b1
      b2' <- typecheck (me, mt) b2
      if exprType c' /= TInt
        then throwError $ ConditionalNotInt expr
      else if exprType b1' /= exprType b2'
        then throwError $ IfBranchNotSameType expr
        else return $ EIf (exprType b1') c' b1' b2'
    typecheck' (me, mt) expr@(ELambda _ n t e) = do
      case Map.lookup n me of
        Just _ -> throwError $ DuplicateVariable expr n
        Nothing -> do
          e' <- typecheck (Map.insert n t me, mt) e
          return $ ELambda (TFunc t $ exprType e') n t e'
    typecheck' (me, mt) expr@(EApp _ f t) = do
      f' <- typecheck (me, mt) f
      t' <- typecheck (me, mt) t
      case exprType f' of
        TFunc tau sigma ->
          if tau /= exprType t'
            then throwError $ FunctionTypeMismatch expr
            else return $ EApp sigma f' t'
        _ -> throwError $ NonFunctionApplication expr
    typecheck' (me, mt) expr@(ETupleGet _ e i) = do
      e' <- typecheck (me, mt) e
      case exprType e' of
        TTuple ts ->
          if 0 <= i && length ts > i
            then return $ ETupleGet (ts !! i) e' i
            else throwError $ BadTupleIndex expr
        _ -> throwError $ ExceptedTuple expr
    typecheck' (me, mt) expr@(ETuple _ es) = do
      es' <- typecheck (me, mt) `mapM` es
      return $ ETuple (TTuple $ exprType <$> es') es'
    typecheck' (me, mt) expr@(EListFold _ f x) = do
      f' <- typecheck (me, mt) f
      x' <- typecheck (me, mt) x
      case exprType f' of
        TFunc a (TFunc sigma sigma') -> 
          if sigma == sigma'
            then return $ EListFold (TFunc (TList a) sigma) f' x'
            else throwError $ FunctionTypeMismatch expr
        _ -> throwError $ NonFunctionApplication expr
    typecheck' (me, mt) expr@(EListCons _ x xs) = do
      x' <- typecheck (me, mt) x
      xs' <- typecheck (me, mt) xs
      if exprType xs' == TList (exprType x')
        then return $ EListCons (TList (exprType x')) x' xs'
        else throwError $ FunctionTypeMismatch expr
    typecheck' (me, mt) expr@(EListEmpty _ t) = do
      return $ EListEmpty (TList t) t


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
compile (EConst _ i)          = tell [LDC $ VInt i]
compile (EVar _ s)            = do
  state <- ask
  case Map.lookup s (variableLocation state) of
    Just (VLEnvironment n i) -> tell [LD (VInt n) (VInt i)]
    Just (VLGlobal lbl)      -> tell [LDC (VLabel lbl)]
    Just (VLConstant i)      -> tell [LDC (VInt i)]
compile (ELet _ n v e)        = compile (EApp (exprType e) (ELambda (TFunc (exprType v) (exprType e)) n (exprType v) e) v)
compile (EType _ n v e)       = compile e
compile (EAdd _ e1 e2)        = do
  compile e1
  compile e2
  tell [ADD]
compile (ESub _ e1 e2)        = do
  compile e1
  compile e2
  tell [SUB]
compile (EMul _ e1 e2)        = do
  compile e1
  compile e2
  tell [MUL]
compile (EDiv _ e1 e2)        = do
  compile e1
  compile e2
  tell [DIV]
compile (EEq _ e1 e2)         = do
  compile e1
  compile e2
  tell [CEQ]
compile (ENEq _ e1 e2)        = error "unimplemented ENEq"
compile (EGTE _ e1 e2)        = do
  compile e1
  compile e2
  tell [CGTE]
compile (EGT _ e1 e2)         = do
  compile e1
  compile e2
  tell [CGT]
compile (ELT _ e1 e2)         = error "unimplemented ELT"
compile (ELTE _ e1 e2)        = error "unimplemented ELTE"
compile (EIf _ c b1 b2)      = do
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
compile (ELambda _ n t e)     = do
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
compile (EApp _ f t)          = do
  compile t -- t is on stack
  compile f -- f closure is on stack
  tell [AP $ VInt 1]
compile (ETupleGet _ e i)    = do
  compile e
  case exprType e of
    TTuple ts -> compileTupleGet i ts
  where
    compileTupleGet 0 (t:[]) = return ()
    compileTupleGet 0 (t:ts) = tell [CAR]
    compileTupleGet i (t:ts) = do
      tell [CDR]
      compileTupleGet (i-1) ts
compile (ETuple _ [])         = error "unimplemented ETuple (empty)"
compile (ETuple _ (e:es))     = compileTuple (e:es)
  where
    compileTuple (e1:e2:[]) = do
      compile e1
      compile e2
      tell [CONS]
    compileTuple (e:es)     = do
      compile e
      compileTuple es
      tell [CONS]
compile (EListFold _ f x)         = do
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

  tell [LDC $ VInt 0, TSEL (VLabel lambdaEnd) (VLabel lambdaEnd)]
  block blockName $ bindVariable "?" $ do
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
        , LDF (VLabel bbegin)
        , TAP (VInt 1)
        ]
  block lambdaBlock $ bindVariable "?" $ do
    compile x
    tell
      [ LD (VInt 0) (VInt 0)
      , CONS
      , LDF (VLabel bbegin)
      , TAP (VInt 1)
      , RTN
      ]   
  tell [LDF (VLabel lambdaBegin)]
compile (EListCons _ x xs)         = do
  compile x
  compile xs
  tell [CONS]
compile (EListEmpty _ _)           = tell [LDC (VInt 0)]

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