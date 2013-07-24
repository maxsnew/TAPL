module Parser where

import Grammar

import Control.Applicative ((<$>), (<|>), (*>), pure)
import Control.Monad.Identity (Identity)
import Prelude hiding (abs)
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Expr
import Text.Parsec.Token hiding (parens, lexeme, identifier, reserved, reservedOp, braces, brackets, whiteSpace)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

type Parser a = ParsecT String () Identity a

typ :: Parser Type
typ = buildExpressionParser table $
      (reserved "Bool" *> pure TyBool)
      <|> parens typ

table :: OperatorTable String () Identity Type
table = [[Infix (reservedOp "->" *> return TyArr) AssocRight]]

-- | Lexing
lambdaCal :: TokenParser st
lambdaCal = makeTokenParser lambdaStyle

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lambdaCal
parens :: Parser a -> Parser a
parens = lexeme . P.parens lambdaCal
identifier :: Parser String
identifier = lexeme $ P.identifier lambdaCal
reserved :: String -> Parser ()
reserved = lexeme . P.reserved lambdaCal
reservedOp :: String -> Parser ()
reservedOp = lexeme . P.reservedOp lambdaCal

lambdaStyle :: LanguageDef st
lambdaStyle = emptyDef {
    commentStart   = ""
  , commentEnd     = ""
  , commentLine    = "#"
  , nestedComments = False
  , identStart     = letter <|> char '_'
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = oneOf ".-:"
  , opLetter       = oneOf ""
  , reservedOpNames= ["->", ".", ":"]
  , reservedNames  = ["lambda", "λ", "Bool"]
  , caseSensitive  = True
  }
