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
    EConst Int
  | EVar String
  | ELet String Expr Expr
  | EType String Type Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EEq Expr Expr
  | ENEq Expr Expr
  | ELT Expr Expr
  | ELTE Expr Expr
  | EGT Expr Expr
  | EGTE Expr Expr
  | EIf Expr Expr Expr
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
  show (EEq e1 e2)     = "(" ++ show e1 ++ ")" ++ " == " ++ "(" ++ show e2 ++ ")"
  show (ENEq e1 e2)    = "(" ++ show e1 ++ ")" ++ " /= " ++ "(" ++ show e2 ++ ")"
  show (EGTE e1 e2)    = "(" ++ show e1 ++ ")" ++ " >= " ++ "(" ++ show e2 ++ ")"
  show (EGT e1 e2)     = "(" ++ show e1 ++ ")" ++ " > " ++ "(" ++ show e2 ++ ")"
  show (ELT e1 e2)     = "(" ++ show e1 ++ ")" ++ " < " ++ "(" ++ show e2 ++ ")"
  show (ELTE e1 e2)    = "(" ++ show e1 ++ ")" ++ " <= " ++ "(" ++ show e2 ++ ")"
  show (EIf c b1 b2)  = "if " ++ show c ++ " then " ++ show b1 ++ " else " ++ show b2
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
    typecheck' (me, mt) expr@(EEq e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(ENEq e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(EGT e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(EGTE e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(ELTE e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(ELT e1 e2)   = do
      e1Ty <- typecheck (me, mt) e1
      e2Ty <- typecheck (me, mt) e2
      if e1Ty == TInt && e2Ty == TInt
        then return TInt
        else throwError $ ArithNotInt
    typecheck' (me, mt) expr@(EIf c b1 b2) = do
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
compile (EConst i)          = tell [LDC $ VInt i]
compile (EVar s)            = do
  state <- ask
  case Map.lookup s (variableLocation state) of
    Just (VLEnvironment n i) -> tell [LD (VInt n) (VInt i)]
    Just (VLGlobal lbl)      -> tell [LDC (VLabel lbl)]
    Just (VLConstant i)      -> tell [LDC (VInt i)]
compile (ELet n v e)        = compile (EApp (ELambda n TOther e) v)
compile (EType n v e)       = compile e
compile (EAdd e1 e2)        = do
  compile e1
  compile e2
  tell [ADD]
compile (ESub e1 e2)        = do
  compile e1
  compile e2
  tell [SUB]
compile (EMul e1 e2)        = do
  compile e1
  compile e2
  tell [MUL]
compile (EDiv e1 e2)        = do
  compile e1
  compile e2
  tell [DIV]
compile (EEq e1 e2)         = do
  compile e1
  compile e2
  tell [CEQ]
compile (ENEq e1 e2)        = error "unimplemented ENEq"
compile (EGTE e1 e2)        = do
  compile e1
  compile e2
  tell [CGTE]
compile (EGT e1 e2)         = do
  compile e1
  compile e2
  tell [CGT]
compile (ELT e1 e2)         = error "unimplemented ELT"
compile (ELTE e1 e2)        = error "unimplemented ELTE"
compile (EIf c b1 b2)      = do
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
compile (ELambda n t e)     = do
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
compile (EApp f t)          = do
  compile t -- t is on stack
  compile f -- f closure is on stack
  tell [AP $ VInt 1]
compile (ETupleGet e i)     = error "unimplemented ETupleGet"
compile (ETuple [])         = error "unimplemented ETuple (empty)"
compile (ETuple (e:es))     = compileTuple (e:es)
  where
    compileTuple (e1:e2:[]) = do
      compile e1
      compile e2
      tell [CONS]
    compileTuple (e:es)     = do
      compile e
      compileTuple es
      tell [CONS]

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