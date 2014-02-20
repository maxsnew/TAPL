{-# LANGUAGE ViewPatterns #-}
module Checker where

import Grammar

import Data.Map
import Control.Applicative
import Control.Monad.Reader

data TypeError
  = BadApp { expected :: Type, received :: Type }
  | NotAFn { got :: Type }
  deriving (Show)

type Ctx = (Map ExpN Type, Set Type)
type TypeM = ReaderT Ctx (EitherT TypeError FreshM)
typeof :: Term -> TypeM Type
typeof t = case t of
  EVar n -> lookup n <=< fmap fst <=< ask
  Lam bnd -> do
    ((n , unembed -> ty), bod) <- unbind bnd
    rgt <- local (Map.insert n ty) $ typeof bod
    return $ TyArr ty rgt
  LAM bnd -> do
    (n, bod) <- undbind bnd
    inner <- typeof bod
    return $ TyLam (bind n inner)
  App e1 e2 -> do
    t1 <- typeof e1
    t2 <- typeof e2
    case t1 of
      TyArr t11 t12 -> do
        if t11 == t2
          then return t12
          else throwError $ BadApp t11 t2
      _ -> throwError $ NotAFn t1
  TyApp e t2 -> do
    t1 <- typeof e
    case t1 of
      TyLam bnd -> do
        (tyx, tbod) <- unbind bnd
        return $ subst x t2 tbod
