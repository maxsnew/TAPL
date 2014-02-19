module Main where

import Grammar
import Parser

import Control.Monad.IO.Class
import System.Console.Haskeline
import Unbound.LocallyNameless

type ReplM = InputT IO

main :: IO ()
main = runInputT defaultSettings repl

repl :: ReplM ()
repl = do
  inp <- getInputLine "· ⊢ "
  case inp of
    Nothing -> return ()
    Just t -> do
      let me = runFreshM $ defRunParser expr t
      liftIO $ case me of
        Left err -> print me
        Right t -> print t
      repl
