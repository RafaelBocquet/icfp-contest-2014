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
  a <- foldM programLabelMap' (0, Map.empty) pr
  throwError $ BadLabel ""
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
  return $ "LDC " ++ x' ++ " " ++ y' ++ "\n"
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
