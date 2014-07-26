module Main where

import Options.Applicative
import Control.Applicative
import Control.Monad

import Lib.GCC as GCC
import Lib.GHC as GHC
import Lib.GHCParser as GHC hiding (Parser)
import Lib.HL as HL
import Lib.HLParser as HL hiding (Parser)

ghc :: String -> IO ()
ghc filename = do
  fileContents <- readFile filename
  case GHC.parse fileContents of
    Left err -> putStrLn . show $ err
    Right pr -> do
      case GHC.showProgram pr of
        Left err -> putStrLn $ show err
        Right s ->  writeFile (filename ++ ".out") s

compile :: String -> IO ()
compile filename = do
  fileContents <- readFile filename
  case HL.parse fileContents of
    Left err -> putStrLn . show $ err
    Right hl -> do
      putStrLn "Parsed : "
      -- putStrLn . show $ hl
      putStrLn "Typechecks to : "
      case typecheck0 hl of
        Left err -> putStrLn . show $ err
        Right hl' -> do
          putStrLn "Compiles to : "
          putStrLn . showLabelProgram . runHL $ HL.fullCompile hl'
          putStrLn "UNLABEL :"
          case GCC.showProgram . runHL $ HL.fullCompile hl' of
            Left err -> putStrLn . show $ err
            Right s -> writeFile (filename ++ ".out") s

pacman :: String -> IO ()
pacman filename = do
  fileContents <- readFile filename
  case HL.parse fileContents of
    Left err -> putStrLn . show $ err
    Right hl -> do
      putStrLn "Parsed : "
      -- putStrLn . show $ hl
      putStrLn "Typechecks to : "
      case typecheck0 hl of
        Left err -> putStrLn . show $ err
        Right hl' -> do
          putStrLn "Compiles to : "
          -- putStrLn . showLabelProgram . runHL $ HL.toPacman hl'
          putStrLn "UNLABEL :"
          case GCC.showProgram . runHL $ HL.toPacman hl' of
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
  <> (command "ghc" $ info (
        Main.ghc <$> argument str idm
      ) idm)

main :: IO ()
main = do
  putStrLn "ICFP Contest 2014"
  putStrLn "Rafaël Bocquet & Simon Mauras"
  putStrLn "============================="
  join $ execParser (info optionParser idm)