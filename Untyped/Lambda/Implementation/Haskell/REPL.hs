module Main where

import Grammar (removeNames, restoreNames)
import Interpreter (eval')
import qualified Parser (term)
import qualified Pretty (namedTerm)

import Control.Monad (forever)
import Data.Functor ((<$>))
import System.IO (hFlush, stdout)
import Text.Parsec (parse)

main :: IO ()
main = forever repl

repl :: IO ()
repl = do
  putStr "λ: "
  hFlush stdout
  line <- getLine
  putStrLn $ case parse Parser.term "repl" line of
    Left err -> show err
    Right nt  -> let (t, ctx) = removeNames nt
                     reduced  = eval' t
                     maybeNt      = flip restoreNames ctx <$> reduced in
                 case maybeNt of
                   Nothing -> "No reduction"
                   Just nt' -> show $ Pretty.namedTerm nt'
