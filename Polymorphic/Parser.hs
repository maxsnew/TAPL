{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Grammar

import Control.Applicative
import Control.Monad.Trans
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Expr
import Text.Parsec.Token hiding (parens, lexeme, identifier, reserved, reservedOp, braces, brackets, whiteSpace, natural)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Unbound.LocallyNameless

type Parser a = ParsecT String () FreshM a

instance (Fresh m) => Fresh (ParsecT String () m) where
  fresh = lift . fresh

defRunParser :: Parser a -> String -> FreshM (Either ParseError a)
defRunParser p inp = runParserT p () "REPL" inp

freshVar :: (Rep a) => Parser (Name a)
freshVar = do
  x <- identifier
  return $ string2Name x

-- | Terms
expr :: Parser Expr
expr = lexeme $ bod <|> parens expr

var :: Parser Expr
var = (EVar <$> freshVar)
      <?> "variable"

lam :: Parser Expr
lam = do
  string "\\"
  v <- freshVar
  reservedOp ":"
  ty <- typ
  reservedOp "."
  e <- bod
  return $ Lam (bind (v, ty) e)
  <?> "λ abstraction"

lAM :: Parser Expr
lAM = do
  string "/\\"
  v <- freshVar
  reservedOp "."
  e <- bod
  return $ LAM (bind v e)

bod = lam <|> lAM <|> apps

apps :: Parser Expr
apps = do
  ts <- many1 (var <|> parens expr)
  return $ case ts of
    [t]    -> t
    (t:ts') -> foldl App t ts'
  <?> "function application"

-- | Types
typ :: Parser Type
typ = (tyLam <|> tyBod <|> parens typ)
      <?> "type"

tyLam :: Parser Type
tyLam = do
  string "\\/"
  v <- freshVar
  reservedOp "."
  t <- tyBod
  return $ TyLam (bind v t)

tyBod = tyLam <|> tyArrs

tyArrs :: Parser Type
tyArrs = do
  ts <- (tyVar <|> parens typ) `sepBy1` (reservedOp "->")
  return $ case ts of
    [t] -> t
    (_:_) -> foldr1 TyArr ts

tyVar :: Parser Type
tyVar = (TyVar <$> freshVar)
        <?> "type variable"

-- table :: OperatorTable String () Identity Type
-- table = [ [Infix (reservedOp "->" *> return TyArr) AssocRight] ]

-- | Lexing
lambdaCal :: GenTokenParser String u FreshM
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

lambdaStyle :: GenLanguageDef String u FreshM
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
  , reservedNames  = ["unit", "Unit", "Bool", "true", "false",
                       "if", "then", "else", "Nat"]
  , caseSensitive  = True
  }
