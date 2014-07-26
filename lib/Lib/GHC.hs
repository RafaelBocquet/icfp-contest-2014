module Lib.GHC where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable (foldl')

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Register = RA | RB | RC | RD | RE | RF | RG | RH | RPC

instance Show Register where
  show RA = "A"
  show RB = "B"
  show RC = "C"
  show RD = "D"
  show RE = "E"
  show RF = "F"
  show RG = "G"
  show RH = "H"
  show RPC = "PC"

data Data =
    Register Register
  | MRegister Register
  | MConstant Int
  | Constant Int
  | Label String

instance Show Data where
  show (Register a)  = show a 
  show (MRegister a) = "[" ++ show a ++ "]"
  show (MConstant a) = "[" ++ show a ++ "]"
  show (Constant a)  = show a
  show (Label a)     = "%" ++ show a

data Interupt =
    OutGhostDirection
  | LambdaMan1
  | GhostIndex
  | GhostStart
  | GhostPosition
  | GhostDirection
  | MapData
  | Debug

instance Show Interupt where
  show OutGhostDirection  = show 0
  show LambdaMan1         = show 1
  show GhostIndex         = show 3
  show GhostStart         = show 4
  show GhostPosition      = show 5
  show GhostDirection     = show 6
  show MapData            = show 7
  show Debug              = show 8

data Instruction =
    MOV Data Data
  | INC Data
  | DEC Data
  | ADD Data Data
  | SUB Data Data
  | MUL Data Data
  | DIV Data Data
  | AND Data Data
  | XOR Data Data
  | JLT Data Data Data
  | JEQ Data Data Data
  | JGT Data Data Data
  | INT Interupt
  | HLT
  | LABEL String

instance Show Instruction where
  show (MOV a b)    = "  MOV " ++ show a ++ "," ++ show b ++ "\n" 
  show (INC a)      = "  INC " ++ show a ++ "\n" 
  show (DEC a)      = "  DEC " ++ show a ++ "\n" 
  show (ADD a b)    = "  ADD " ++ show a ++ "," ++ show b ++ "\n" 
  show (SUB a b)    = "  SUB " ++ show a ++ "," ++ show b ++ "\n" 
  show (MUL a b)    = "  MUL " ++ show a ++ "," ++ show b ++ "\n" 
  show (DIV a b)    = "  DIV " ++ show a ++ "," ++ show b ++ "\n" 
  show (AND a b)    = "  AND " ++ show a ++ "," ++ show b ++ "\n" 
  show (XOR a b)    = "  XOR " ++ show a ++ "," ++ show b ++ "\n" 
  show (JLT a b c)  = "  JLT " ++ show a ++ "," ++ show b ++ "," ++ show c ++ "\n" 
  show (JEQ a b c)  = "  JEQ " ++ show a ++ "," ++ show b ++ "," ++ show c ++ "\n" 
  show (JGT a b c)  = "  JGT " ++ show a ++ "," ++ show b ++ "," ++ show c ++ "\n" 
  show (INT a)      = "  INT " ++ show a ++ "\n"
  show HLT          = "  HLT\n"
  show (LABEL s)    = s ++ ":\n"

data Error =
    BadLabel String
  | DuplicateLabel String
  deriving(Show)

programLabelMap :: [Instruction] -> Either Error (Map String Int)
programLabelMap pr = do
  snd <$> foldM programLabelMap' (0, Map.empty) pr
  where
    programLabelMap' :: (Int, Map String Int) -> Instruction -> Either Error (Int, Map String Int)
    programLabelMap' (i, mp) (LABEL "") = return (i, mp)
    programLabelMap' (i, mp) (LABEL l) = case Map.lookup l mp of
      Nothing -> return (i, Map.insert l i mp)
      Just _ -> throwError $ DuplicateLabel l
    programLabelMap' (i, mp) _         = return (i+1, mp)

unlabelData :: Map String Int -> Data -> Either Error Data
unlabelData mp (Label str) = case Map.lookup str mp of
  Just x  -> return $ Constant x
  Nothing -> throwError $ BadLabel str
unlabelData mp d = return d

unlabelInstruction :: Map String Int -> Instruction -> Either Error Instruction
unlabelInstruction mp (MOV a b)    = (liftM2 MOV) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (INC a)      = (liftM INC) (unlabelData mp a)
unlabelInstruction mp (DEC a)      = (liftM DEC) (unlabelData mp a)
unlabelInstruction mp (ADD a b)    = (liftM2 ADD) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (SUB a b)    = (liftM2 SUB) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (MUL a b)    = (liftM2 MUL) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (DIV a b)    = (liftM2 DIV) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (AND a b)    = (liftM2 AND) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (XOR a b)    = (liftM2 XOR) (unlabelData mp a) (unlabelData mp b)
unlabelInstruction mp (JLT a b c)  = (liftM3 JLT) (unlabelData mp a) (unlabelData mp b) (unlabelData mp c)
unlabelInstruction mp (JEQ a b c)  = (liftM3 JEQ) (unlabelData mp a) (unlabelData mp b) (unlabelData mp c)
unlabelInstruction mp (JGT a b c)  = (liftM3 JGT) (unlabelData mp a) (unlabelData mp b) (unlabelData mp c)
unlabelInstruction mp HLT          = return $ HLT
unlabelInstruction mp (INT a)          = return $ INT a
unlabelInstruction mp (LABEL s)    = return $ LABEL s

isLabel :: Instruction -> Bool
isLabel (LABEL _) = True
isLabel _         = False

showProgram :: [Instruction] -> Either Error String
showProgram pr = do
  labelMap <- programLabelMap pr
  unlabeled <- unlabelInstruction labelMap `mapM` pr
  return $ concat $ show <$> filter (not . isLabel) unlabeled