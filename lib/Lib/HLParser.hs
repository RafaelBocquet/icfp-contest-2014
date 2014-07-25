module Lib.HLParser where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.Monad.Except
import Data.Foldable (foldl')

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lib.HL

import Control.Monad.Identity

import Text.ParserCombinators.Parsec.Char
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Token
import Text.Parsec.Language

type Parser a = Parsec String () a

hlLanguageDef :: LanguageDef st
hlLanguageDef = emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter <|> char '_'
  , identLetter     = alphaNum <|> char '_'
  , opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedNames   =
      [ "I"
      , "let"
      , "in"
      , "if"
      , "then"
      , "else"
      ]
  , reservedOpNames =
      [ "->"
      , "."
      , ":"
      , "\\"
      , "?"
      , "+", "-", "/", "*"
      , ";", "="
      , "==", "/=", "<=", ">=", "<", ">"
      ]
  , caseSensitive = False
  }

hlTokenParser = makeTokenParser hlLanguageDef

TokenParser {
  identifier    = idParser,
  reserved      = reParser,
  reservedOp    = reOpParser,
  stringLiteral = strParser,
  parens        = parParser,
  brackets      = braParser,
  lexeme        = lexParser,
  commaSep1     = commaParser,
  integer       = intParser,
  whiteSpace    = wsParser
} = hlTokenParser

parseType :: Parser Type
parseType = do
  leftType <- choice
    [ lexParser $ do
        reParser "I"
        return TInt
    , lexParser $ do
        reOpParser "?"
        return TOther
    , TVar <$> lexParser idParser
    , lexParser $ parParser $ do
        ts <- commaParser parseType
        return $ case ts of
          []   -> TOther
          t:[] -> t
          ts   -> TTuple ts
    , lexParser $ braParser $ do
        ty <- parseType
        return $ TList ty
    ]
  choice
    [ do
        try $ lexParser $ reOpParser "->"
        rightType <- parseType
        return $ TFunc leftType rightType
    , return leftType
    ]

parseExpression4 :: Parser Expr
parseExpression4 = choice
  [ lexParser $ parParser $ do
      ts <- commaParser parseExpression
      return $ case ts of
        []   -> ETuple []
        e:[] -> e
        es   -> ETuple es
  , EConst . fromIntegral <$> lexParser intParser
  , EVar <$> lexParser idParser
  ]

parseExpression3 :: Parser Expr
parseExpression3 = do
  e1 <- parseExpression4
  choice
    [ do
        e2 <- parseExpression3
        return $ EApp e1 e2
    , do
        i <- braParser intParser
        return $ ETupleGet e1 (fromIntegral i)
    , return e1
    ]

parseExpression2 :: Parser Expr
parseExpression2 = do
  e1 <- parseExpression3
  choice
    [ do
        lexParser $ reOpParser "+"
        e2 <- parseExpression2
        return $ EAdd e1 e2
    , do
        lexParser $ reOpParser "-"
        e2 <- parseExpression2
        return $ ESub e1 e2
    , return e1
    ]

parseExpression1 :: Parser Expr
parseExpression1 = do
  e1 <- parseExpression2
  choice
    [ do
        lexParser $ reOpParser "*"
        e2 <- parseExpression1
        return $ EMul e1 e2
    , do
        lexParser $ reOpParser "/"
        e2 <- parseExpression1
        return $ EDiv e1 e2
    , return e1
    ]

parseExpression0 :: Parser Expr
parseExpression0 = do
  e1 <- parseExpression1
  choice
    [ do
        lexParser $ reOpParser "=="
        e2 <- parseExpression1
        return $ EEq e1 e2
    , do
        lexParser $ reOpParser "/="
        e2 <- parseExpression1
        return $ ENEq e1 e2
    , do
        lexParser $ reOpParser "<="
        e2 <- parseExpression1
        return $ ELTE e1 e2
    , do
        lexParser $ reOpParser "<"
        e2 <- parseExpression1
        return $ ELT e1 e2
    , do
        lexParser $ reOpParser ">="
        e2 <- parseExpression1
        return $ EGTE e1 e2
    , do
        lexParser $ reOpParser ">"
        e2 <- parseExpression1
        return $ EGT e1 e2
    , return e1
    ]

parseExpression :: Parser Expr
parseExpression = choice
  [ lexParser $ do
      lexParser $ reParser "let"
      x <- idParser
      lexParser $ reOpParser "="
      v <- parseExpression
      lexParser $ reParser "in"
      e <- parseExpression
      return $ ELet x v e
  , lexParser $ do
      lexParser $ reParser "type"
      x <- idParser
      lexParser $ reOpParser "="
      v <- parseType
      lexParser $ reParser "in"
      e <- parseExpression
      return $ EType x v e
  , do
      lexParser $ reParser "if"
      e <- parseExpression
      lexParser $ reParser "then"
      b1 <- parseExpression
      lexParser $ reParser "else"
      b2 <- parseExpression
      return $ EIf e b1 b2
  , do
      lexParser $ reOpParser "\\"
      id <- idParser
      lexParser $ reOpParser ":"
      ty <- parseType
      lexParser $ reOpParser "."
      e <- parseExpression
      return $ ELambda id ty e
  , parseExpression1
  ]

mainParser :: Parser Expr
mainParser = do
  wsParser
  parseExpression

parse :: String -> Either ParseError Expr
parse = runIdentity . runPT mainParser () ""