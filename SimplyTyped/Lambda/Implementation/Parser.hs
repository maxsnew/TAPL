module Parser where

import Grammar

import Control.Applicative ((<$>), (<|>), (*>), pure)
import Control.Monad.Identity (Identity)
import Prelude hiding (abs)
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Expr
import Text.Parsec.Token hiding (parens, lexeme, identifier, reserved, reservedOp, braces, brackets, whiteSpace, natural)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

type Parser a = ParsecT String () Identity a

-- | Terms
term :: Parser NamedTerm
term = lexeme $ val <|> if' <|> abs <|> apps <|> parens term

val :: Parser NamedTerm
val = bool <|> nat

bool :: Parser NamedTerm
bool = resBool "true" NTrue <|> resBool "false" NFalse
  where resBool s nt = reserved s *> return nt

nat :: Parser NamedTerm
nat = NNat <$> natural

if' :: Parser NamedTerm
if' = do
  reserved "if"
  t1 <- term
  reserved "then"
  t2 <- term
  reserved "else"
  t3 <- term
  return $ NIf t1 t2 t3

var :: Parser NamedTerm
var = (NVar <$> identifier)
      <?> "variable"

abs :: Parser NamedTerm
abs = do
  reserved "lambda" <|> reserved "λ"
  v <- identifier
  reservedOp ":"
  ty <- typ
  reservedOp "."
  t <- abs <|> apps
  return $ NAbs ty v t
  <?> "λ abstraction"

apps :: Parser NamedTerm
apps = do
  ts <- many1 (var <|> val <|> parens term)
  return $ case ts of
    [t]    -> t
    (t:ts') -> foldl NApp t ts'
  <?> "function application"

-- | Types
typ :: Parser Type
typ = (buildExpressionParser table $
       resTyp "Bool" TyBool
       <|> resTyp "Nat" TyNat
       <|> parens typ)
      <?> "type"
  where resTyp s t = reserved s *> pure t

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
natural :: Parser Integer
natural = lexeme $ P.natural lambdaCal

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
  , reservedNames  = ["lambda", "λ", "Bool", "Nat", "if", "then", "else", "true", "false"]
  , caseSensitive  = True
  }
