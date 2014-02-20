{-# LANGUAGE ViewPatterns #-}
module Pretty where
 
import Grammar

import Text.PrettyPrint
import Unbound.LocallyNameless hiding (name)

expr :: (Fresh m) => Expr -> m Doc
expr e = case e of
  EVar n  -> return $ name n
  Lam bnd -> do
    ((n, unembed -> ty), e) <- unbind bnd
    let pn = name n
    pty <- typ ty
    pe  <- expr e
    return $ char 'λ' <> pn <> char ':' <> parens pty <> char '.' <+> pe
  LAM bnd -> do
    (n, e) <- unbind bnd
    let pn = name n
    pe <- expr e
    return $ char 'Λ' <> pn <> char '.' <+> pe
  App e1 e2 -> do
    p1 <- expr e1
    p2 <- expr e2
    return $ parens $ p1 <+> p2
  TyApp e t -> do
    pe <- expr e
    pt <- typ  t
    return $ parens $ pe <+> brackets tp

typ :: (Fresh m) => Type -> m Doc
typ t = case t of
  TyVar n -> return $ name n
  TyArr t1 t2 -> do
    p1 <- typ t1
    p2 <- typ t2
    return $ p1 <+> text "->" <+> p2
  TyLam bnd -> do
    (n, ty) <- unbind bnd
    let pn = name n
    pt <- typ ty
    return $ char '∀' <> pn <> char '.' <+> pt

name :: Name a -> Doc
name = text . name2String
