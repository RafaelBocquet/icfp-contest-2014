module Main where

import Options.Applicative
import Control.Applicative
import Control.Monad

import Lib.GCC
import Lib.HL
import Lib.HLParser hiding (Parser)

test1 :: IO ()
test1 = case showProgram
    [ LDC  (VInt 0)
    , LDF  (VLabel "MAIN")
    , CONS
    , RTN
    , LABEL "MAIN"
    , LABEL "MAIN"
    , LDC  (VInt 0)
    , LDC  (VInt 1)
    , CONS
    , RTN
    ] of
  Left err -> putStrLn $ show err
  Right s -> putStrLn s

test2 :: IO ()
test2 = putStrLn $ show (mainType TInt)

compile :: String -> IO ()
compile filename = do
  fileContents <- readFile filename
  case parse fileContents of
    Left err -> putStrLn . show $ err
    Right hl -> do
      putStrLn "Parsed : "
      putStrLn . show $ hl
      putStrLn "Compiles to : "
      putStrLn . show $ typecheck0 hl

optionParser :: Parser (IO ())
optionParser = subparser $
     (command "test1" $ info (pure test1) idm)
  <> (command "test2" $ info (pure test2) idm)
  <> (command "compile" $ info (
        compile <$> argument str idm
      ) idm)

main :: IO ()
main = do
  putStrLn "ICFP Contest 2014"
  putStrLn "Rafaël Bocquet & Simon Mauras"
  putStrLn "============================="
  join $ execParser (info optionParser idm)