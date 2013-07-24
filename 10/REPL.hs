module Main where

import Grammar (removeNames, restoreNames)
import Interpreter (eval')
import Checker (typeof)
import qualified Parser (term)
import qualified Pretty (typ, namedTerm)

import Control.Monad (forever)
import Data.Functor ((<$>))
import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import Text.PrettyPrint (parens)

main :: IO ()
main = forever repl

repl :: IO ()
repl = do
  putStr "λ: "
  hFlush stdout
  line <- getLine
  putStrLn $ case parse Parser.term "repl" line of
    Left err -> show err
    Right nt  -> let (t, ctx) = removeNames nt in
      case typeof t of
        Nothing -> "Type Error"
        Just ty -> let reduced  = eval' t
                       maybeNt  = flip restoreNames ctx <$> reduced in
                 case maybeNt of
                   Nothing -> "No reduction! (Must be free variables)"
                   Just nt' -> (show . parens $ Pretty.namedTerm nt')
                               ++ " : "
                               ++ (show $ Pretty.typ ty)
