module Main where

import qualified Parser
import qualified Pretty

import Control.Monad (forever)
import System.IO
import Text.Parsec

main :: IO ()
main = forever repl

repl :: IO ()
repl = do
  putStr "λ: "
  hFlush stdout
  line <- getLine
  putStrLn $ case parse Parser.term "repl" line of
    Left err -> show err
    Right t  -> show $ Pretty.namedTerm t
