module Main where

import Checker
import Grammar
import qualified Parser
import qualified Pretty

import Control.Applicative
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
      liftIO $ putStrLn $ runFreshM $ interp t
      repl

interp :: String -> FreshM String
interp s = do
  me <- Parser.defRunParser Parser.expr s
  case me of
    Left err -> return $ show err
    Right e  -> show <$> Pretty.expr e
