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
      , "fold"
      , "with"
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
  natural       = intParser,
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
        []   -> ETuple TOther []
        e:[] -> e
        es   -> ETuple TOther es
  , EVar TOther <$> lexParser idParser
  , EConst TOther . fromIntegral <$> lexParser intParser
  ]

parseApplications :: Maybe Expr -> Expr -> Parser Expr
parseApplications e1 e2 = choice
  [ do
      e3 <- parseExpression4
      case e1 of Nothing -> parseApplications (Just e2) e3; Just x -> parseApplications (Just $ EApp TOther x e2) e3
  , do
      i <- braParser intParser
      parseApplications e1 $ ETupleGet TOther e2 (fromIntegral i)
  , return $ case e1 of Nothing -> e2 ; Just x -> EApp TOther x e2
  ]

parseExpression3 :: Parser Expr
parseExpression3 = do
  e1 <- parseExpression4
  choice
    [ parseApplications Nothing e1
    , return e1
    ]

parseExpression2 :: Parser Expr
parseExpression2 = do
  e1 <- parseExpression3
  choice
    [ do
        lexParser $ reOpParser "*"
        e2 <- parseExpression2
        return $ EMul TOther e1 e2
    , do
        lexParser $ reOpParser "/"
        e2 <- parseExpression2
        return $ EDiv TOther e1 e2
    , return e1
    ]

parseExpression1 :: Parser Expr
parseExpression1 = do
  e1 <- parseExpression2
  choice
    [ do
        lexParser $ reOpParser "+"
        e2 <- parseExpression1
        return $ EAdd TOther e1 e2
    , do
        lexParser $ reOpParser "-"
        e2 <- parseExpression1
        return $ ESub TOther e1 e2
    , return e1
    ]

parseExpression0 :: Parser Expr
parseExpression0 = do
  e1 <- parseExpression1
  choice
    [ do
        lexParser $ reOpParser "=="
        e2 <- parseExpression1
        return $ EEq TOther e1 e2
    , do
        lexParser $ reOpParser "/="
        e2 <- parseExpression1
        return $ ENEq TOther e1 e2
    , do
        lexParser $ reOpParser "<="
        e2 <- parseExpression1
        return $ ELTE TOther e1 e2
    , do
        lexParser $ reOpParser "<"
        e2 <- parseExpression1
        return $ ELT TOther e1 e2
    , do
        lexParser $ reOpParser ">="
        e2 <- parseExpression1
        return $ EGTE TOther e1 e2
    , do
        lexParser $ reOpParser ">"
        e2 <- parseExpression1
        return $ EGT TOther e1 e2
    , return e1
    ]

parseExpression :: Parser Expr
parseExpression = choice
  [ do
      lexParser $ reParser "let"
      x <- idParser
      lexParser $ reOpParser "="
      v <- parseExpression
      lexParser $ reParser "in"
      e <- parseExpression
      return $ ELet TOther x v e
  , do
      lexParser $ reParser "type"
      x <- idParser
      lexParser $ reOpParser "="
      v <- parseType
      lexParser $ reParser "in"
      e <- parseExpression
      return $ EType TOther x v e
  , do
      lexParser $ reParser "if"
      e <- parseExpression
      lexParser $ reParser "then"
      b1 <- parseExpression
      lexParser $ reParser "else"
      b2 <- parseExpression
      return $ EIf TOther e b1 b2
  , do
      lexParser $ reOpParser "\\"
      id <- idParser
      lexParser $ reOpParser ":"
      ty <- parseType
      lexParser $ reOpParser "."
      e <- parseExpression
      return $ ELambda TOther id ty e
  , lexParser $ do
      lexParser $ reParser "fold"
      f <- parseExpression
      lexParser $ reParser "with"
      x <- parseExpression
      return $ EListFold TOther f x
  , lexParser $ do
      lexParser $ reParser "cons"
      f <- parseExpression
      lexParser $ reParser "with"
      x <- parseExpression
      return $ EListCons TOther f x
  , lexParser $ do
      lexParser $ reParser "empty"
      t <- parseType
      return $ EListEmpty TOther t
  , lexParser $ do
      lexParser $ reParser "head"
      l <- parseExpression
      return $ EListHead TOther l
  , lexParser $ do
      lexParser $ reParser "tail"
      l <- parseExpression
      return $ EListTail TOther l
  , parseExpression0
  ]

mainParser :: Parser Expr
mainParser = do
  wsParser
  parseExpression

parse :: String -> Either ParseError Expr
parse = runIdentity . runPT mainParser () ""