module Parser where

import Grammar

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Identity (Identity)
import Prelude hiding (abs)
import Text.Parsec hiding((<|>), many)
import Text.Parsec.Token hiding (parens, lexeme, identifier, reserved, braces, brackets, whiteSpace)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

type Parser a = ParsecT String () Identity a

-- | Parsing
term :: Parser NamedTerm
term = lexeme $ abs <|> apps <|> parens term

var :: Parser NamedTerm
var = (NVar <$> identifier)
      <?> "variable"

abs :: Parser NamedTerm
abs = do
  reserved "lambda" <|> reserved "λ"
  v <- identifier
  lexeme . char $ '.'
  t <- abs <|> apps
  return $ NAbs v t

apps :: Parser NamedTerm
apps = do
  ts <- many1 (var <|> parens term)
  return $ case ts of
    [t]    -> t
    (t:ts') -> foldl NApp t ts'

-- | Lexing
lambdaCal :: TokenParser st
lambdaCal = makeTokenParser lambdaStyle

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lambdaCal
parens :: Parser a -> Parser a
parens = lexeme . (P.parens lambdaCal)
identifier :: Parser String
identifier = lexeme $ P.identifier lambdaCal
reserved :: String -> Parser ()
reserved = lexeme . (P.reserved lambdaCal)

lambdaStyle :: LanguageDef st
lambdaStyle = emptyDef {
    commentStart   = ""
  , commentEnd     = ""
  , commentLine    = "#"
  , nestedComments = False
  , identStart     = letter <|> char '_'
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = fail "no ops"
  , opLetter       = fail "no ops"
  , reservedOpNames= []
  , reservedNames  = ["lambda", "λ"]
  , caseSensitive  = True
  }
