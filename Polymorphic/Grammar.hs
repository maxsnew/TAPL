{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Grammar where

{-| System F -}
import Unbound.LocallyNameless

-- Expression Variable Name
type ExpN = Name Expr
-- Type Variable Name
type TypN = Name Type
data Expr =
    EVar ExpN
  | Lam (Bind (ExpN, Embed Type) Expr)
  | LAM (Bind TypN Expr)
  | App   Expr Expr
  | TyApp Expr Type
  deriving (Show)

data Type =
    TyVar TypN
  | TyArr Type Type
  | TyLam (Bind TypN Type)
  deriving (Show)

instance Alpha Expr where
instance Alpha Type where

instance Subst Expr Expr where
  isVar (EVar x) = Just (SubstName x)
  isVar _        = Nothing

instance Subst Expr Type where
  isVar _ = Nothing

instance Subst Type Type where
  isVar (TyVar x) = Just (SubstName x)
  isVar _         = Nothing
         
  
$(derive [''Expr, ''Type])
