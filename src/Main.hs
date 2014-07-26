module Main where

import Options.Applicative
import Control.Applicative
import Control.Monad

import Lib.GCC
import Lib.HL as HL
import Lib.HLParser hiding (Parser)

compile :: String -> IO ()
compile filename = do
  fileContents <- readFile filename
  case parse fileContents of
    Left err -> putStrLn . show $ err
    Right hl -> do
      putStrLn "Parsed : "
      putStrLn . show $ hl
      putStrLn "Typechecks to : "
      case typecheck0 hl of
        Left err -> putStrLn . show $ err
        Right hl' -> do
          putStrLn "Compiles to : "
          putStrLn . showLabelProgram . runHL $ HL.fullCompile hl'
          putStrLn "UNLABEL :"
          case showProgram . runHL $ HL.fullCompile hl' of
            Left err -> putStrLn . show $ err
            Right s -> putStrLn s

pacman :: String -> IO ()
pacman filename = do
  fileContents <- readFile filename
  case parse fileContents of
    Left err -> putStrLn . show $ err
    Right hl -> do
      putStrLn "Parsed : "
      putStrLn . show $ hl
      putStrLn "Typechecks to : "
      case typecheck0 hl of
        Left err -> putStrLn . show $ err
        Right hl' -> do
          putStrLn "Compiles to : "
          putStrLn . showLabelProgram . runHL $ HL.toPacman hl'
          putStrLn "UNLABEL :"
          case showProgram . runHL $ HL.toPacman hl' of
            Left err -> putStrLn . show $ err
            Right s -> writeFile (filename ++ ".out") s

optionParser :: Parser (IO ())
optionParser = subparser $
     (command "compile" $ info (
        Main.compile <$> argument str idm
      ) idm)
  <> (command "pacman" $ info (
        Main.pacman <$> argument str idm
      ) idm)

main :: IO ()
main = do
  putStrLn "ICFP Contest 2014"
  putStrLn "Rafaël Bocquet & Simon Mauras"
  putStrLn "============================="
  join $ execParser (info optionParser idm)