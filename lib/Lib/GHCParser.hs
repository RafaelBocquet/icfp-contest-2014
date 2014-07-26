{-# LANGUAGE FlexibleContexts #-}

module Lib.GHCParser where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.Monad.Except
import Data.Foldable (foldl')

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char

import Lib.GHC

import Control.Monad.Identity

import Text.ParserCombinators.Parsec.Char hiding (space, spaces)
import Text.Parsec hiding (space, spaces)
import Text.Parsec.Pos

type Parser a = Parsec String () a

space :: (Stream s m Char) => ParsecT s u m Char
space               = satisfy (\c -> isSpace c || c == '\n')       <?> "space"
spaces :: (Stream s m Char) => ParsecT s u m ()
spaces              = skipMany space        <?> "white space"

parseRegister :: Parser Register
parseRegister = choice
  [ do
      char 'A'
      return $ RA
  , do
      char 'B'
      return $ RB
  , do
      char 'C'
      return $ RC
  , do
      char 'D'
      return $ RD
  , do
      char 'E'
      return $ RE
  , do
      char 'F'
      return $ RF
  , do
      char 'G'
      return $ RG
  , do
      char 'H'
      return $ RH
  , do
      string "PC"
      return $ RPC
  ]

parseConstant :: Parser Int
parseConstant = do
  s <- many1 digit
  return $ foldl (\acc d -> 10 * acc + d) 0 (digitToInt <$> s)

parseData :: Parser Data
parseData = choice
  [ do
      char '%'
      s <- many1 (letter <|> digit <|> char '.' <|> char '_')
      spaces
      return $ Label s
  , do
      a <- Register <$> parseRegister
      spaces
      return a
  , do
      a <- Constant <$> parseConstant
      spaces
      return a
  , do
      char '['
      spaces
      a <- choice
        [ MRegister <$> parseRegister
        , MConstant <$> parseConstant
        ]
      spaces
      char ']'
      spaces
      return a
  ]

parseInterupt :: Parser Interupt
parseInterupt = choice
  [ try $ do
      string "OutGhostDirection"
      return $ OutGhostDirection
  , try $ do
      string "LambdaMan1"
      return $ LambdaMan1
  , try $ do
      string "GhostIndex"
      return $ GhostIndex
  , try $ do
      string "GhostStart"
      return $ GhostStart
  , try $ do
      string "GhostPosition"
      return $ GhostPosition
  , try $ do
      string "GhostDirection"
      return $ GhostDirection
  , try $ do
      string "MapData"
      return $ MapData
  , try $ do
      string "Debug"
      return $ Debug
  ]

parseInstruction :: Parser Instruction
parseInstruction = choice
  [ do
      try $ string "//"
      manyTill anyChar (char '\n')
      return $ LABEL ""
  , do
      try $ string "MOV"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ MOV a b
  , do
      try $ string "INC"
      spaces
      a <- parseData
      spaces
      return $ INC a
  , do
      try $ string "DEC"
      spaces
      a <- parseData
      spaces
      return $ DEC a
  , do
      try $ string "ADD"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ ADD a b
  , do
      try $ string "SUB"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ SUB a b
  , do
      try $ string "MUL"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ MUL a b
  , do
      try $ string "DIV"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ DIV a b
  , do
      try $ string "AND"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ AND a b
  , do
      try $ string "XOR"
      spaces
      a <- parseData
      b <- parseData
      spaces
      return $ XOR a b
  , do
      try $ string "JLT"
      spaces
      a <- parseData
      b <- parseData
      c <- parseData
      spaces
      return $ JLT a b c
  , do
      try $ string "JEQ"
      spaces
      a <- parseData
      b <- parseData
      c <- parseData
      spaces
      return $ JEQ a b c
  , do
      try $ string "JGT"
      spaces
      a <- parseData
      b <- parseData
      c <- parseData
      spaces
      return $ JGT a b c
  , do
      try $ string "INT"
      spaces
      a <- parseInterupt
      spaces
      return $ INT a
  , do
      try $ string "HLT"
      spaces
      return HLT
  , do
      try $ char '%'
      s <- many1 (letter <|> digit <|> char '.' <|> char '_')
      char ':'
      spaces
      return $ LABEL s
  ]

mainParser :: Parser [Instruction]
mainParser = do
  spaces
  Text.Parsec.many parseInstruction

parse :: String -> Either ParseError [Instruction]
parse = runIdentity . runPT mainParser () ""
