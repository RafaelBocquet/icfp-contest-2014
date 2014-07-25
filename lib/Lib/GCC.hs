module Lib.GCC where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable (foldl')

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Value =
    VInt   Int
  | VLabel String

data Instruction =
    LDC Value | LD Value Value
  | ADD | SUB | MUL | DIV
  | CEQ | CGT | CGTE
  | ATOM | CONS | CAR | CDR
  | SEL Value Value | JOIN
  | LDF Value | AP Value | RTN
  | DUM Value
  | RAP Value
  | STOP
  | TSEL Value Value | TAP | TRAP   -- Tail call extension
  | ST                  -- Pascal extension
  | DBUG | BRK          -- Debug extension

  | LABEL String

data Error =
    BadLabel String
  | DuplicateLabel String
  deriving(Show)

programLabelMap :: [Instruction] -> Either Error (Map String Int)
programLabelMap pr = do
  snd <$> foldM programLabelMap' (0, Map.empty) pr
  where
    programLabelMap' :: (Int, Map String Int) -> Instruction -> Either Error (Int, Map String Int)
    programLabelMap' (i, mp) (LABEL l) = case Map.lookup l mp of
      Nothing -> return (i, Map.insert l i mp)
      Just _ -> throwError $ DuplicateLabel l
    programLabelMap' (i, mp) _         = return (i+1, mp)

showValue :: Map String Int -> Value -> Either Error String
showValue _ (VInt i)   = return $ show i
showValue m (VLabel l) = case Map.lookup l m of
  Nothing -> throwError $ BadLabel l
  Just x  -> return $ show x

showInstruction :: Map String Int -> Instruction -> Either Error String
showInstruction mp (LDC x)      = do
  x' <- showValue mp x
  return $ "LDC " ++ x' ++ "\n"
showInstruction mp (LD x y)     = do
  x' <- showValue mp x
  y' <- showValue mp y
  return $ "LD " ++ x' ++ " " ++ y' ++ "\n"
showInstruction mp (ADD)        = return "ADD\n"
showInstruction mp (SUB)        = return "SUB\n"
showInstruction mp (MUL)        = return "MUL\n"
showInstruction mp (DIV)        = return "DIV\n"
showInstruction mp (CEQ)        = return "CEQ\n"
showInstruction mp (CGT)        = return "CGT\n"
showInstruction mp (CGTE)       = return "CGTE\n"
showInstruction mp (ATOM)       = return "ATOM\n"
showInstruction mp (CONS)       = return "CONS\n"
showInstruction mp (CAR)        = return "CAR\n"
showInstruction mp (CDR)        = return "CDR\n"
showInstruction mp (SEL x y)    = do
  x' <- showValue mp x
  y' <- showValue mp y
  return $ "SEL " ++ x' ++ " " ++ y' ++ "\n"
showInstruction mp (JOIN)       = return "JOIN\n"
showInstruction mp (LDF x)      = do
  x' <- showValue mp x
  return $ "LDF " ++ x' ++ "\n"
showInstruction mp (AP x)      = do
  x' <- showValue mp x
  return $ "AP " ++ x' ++ "\n"
showInstruction mp (RTN)        = return "RTN\n"
showInstruction mp (DUM x)      = do
  x' <- showValue mp x
  return $ "DUM " ++ x' ++ "\n"
showInstruction mp (RAP x)      = do
  x' <- showValue mp x
  return $ "RAP " ++ x' ++ "\n"
showInstruction mp (STOP)       = return "STOP\n"
showInstruction mp (TSEL x y)   = do
  x' <- showValue mp x
  y' <- showValue mp y
  return $ "TSEL " ++ x' ++ " " ++ y' ++ "\n"
showInstruction mp (TAP)        = return "TAP\n"
showInstruction mp (TRAP)       = return "TRAP\n"
showInstruction mp (ST)         = return "ST\n"
showInstruction mp (DBUG)       = return "DBUG\n"
showInstruction mp (BRK)        = return "BRK\n"
showInstruction mp (LABEL _)    = return ""

showProgram :: [Instruction] -> Either Error String
showProgram pr = do
  labelMap <- programLabelMap pr
  concat <$> (showInstruction labelMap) `mapM` pr 

showLabelValue :: Value -> String
showLabelValue (VInt i)   = show i
showLabelValue (VLabel s) = "#" ++ s

showLabelInstruction :: Instruction -> String
showLabelInstruction  (LDC x)      = "LDC " ++ showLabelValue x ++ "\n"
showLabelInstruction  (LD x y)     = "LD " ++ showLabelValue x ++ " " ++ showLabelValue y ++ "\n"
showLabelInstruction  (ADD)        = "ADD\n"
showLabelInstruction  (SUB)        = "SUB\n"
showLabelInstruction  (MUL)        = "MUL\n"
showLabelInstruction  (DIV)        = "DIV\n"
showLabelInstruction  (CEQ)        = "CEQ\n"
showLabelInstruction  (CGT)        = "CGT\n"
showLabelInstruction  (CGTE)       = "CGTE\n"
showLabelInstruction  (ATOM)       = "ATOM\n"
showLabelInstruction  (CONS)       = "CONS\n"
showLabelInstruction  (CAR)        = "CAR\n"
showLabelInstruction  (CDR)        = "CDR\n"
showLabelInstruction  (SEL x y)    = "SEL " ++ showLabelValue x ++ " " ++ showLabelValue y ++ "\n"
showLabelInstruction  (JOIN)       = "JOIN\n"
showLabelInstruction  (LDF x)      = "LDF " ++ showLabelValue x ++ "\n"
showLabelInstruction  (AP x)       = "AP " ++ showLabelValue x ++ "\n"
showLabelInstruction  (RTN)        = "RTN\n"
showLabelInstruction  (DUM x)      = "DUM " ++ showLabelValue x ++ "\n"
showLabelInstruction  (RAP x)      = "RAP " ++ showLabelValue x ++ "\n"
showLabelInstruction  (STOP)       = "STOP\n"
showLabelInstruction  (TSEL x y)   = "TSEL " ++ showLabelValue x ++ " " ++ showLabelValue y ++ "\n"
showLabelInstruction  (TAP)        = "TAP\n"
showLabelInstruction  (TRAP)       = "TRAP\n"
showLabelInstruction  (ST)         = "ST\n"
showLabelInstruction  (DBUG)       = "DBUG\n"
showLabelInstruction  (BRK)        = "BRK\n"
showLabelInstruction  (LABEL s)    = s ++ ":" ++ "\n"

showLabelProgram :: [Instruction] -> String
showLabelProgram pr =
  concat $ showLabelInstruction <$> pr 