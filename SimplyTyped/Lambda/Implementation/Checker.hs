module Checker where

import Grammar

import Data.Functor ((<$>))
import Control.Monad.Reader

typeof :: Term -> Maybe Type
typeof top = runReaderT (rec top) []
  where rec :: Term -> ReaderT [Type] Maybe Type
        rec TTrue  = return TyBool
        rec TFalse = return TyBool
        rec TNat{} = return TyNat
        rec (TIf t1 t2 t3) = do
          ty1 <- rec t1
          guard $ ty1 == TyBool
          ty2 <- rec t2
          ty3 <- rec t3
          guard $ ty2 == ty3
          return ty2
        rec (TVar i) = do
          ctx <- ask
          lift $ ctx !!* i
        rec (TAbs ty t) = TyArr ty <$> (local (ty:) (rec t))
        rec (TApp t1 t2) = do
          ty1 <- rec t1
          ty2 <- rec t2
          case ty1 of
            TyArr ty11 ty12 -> do
              guard $ ty11 == ty2
              return ty12
            _               -> mzero

-- | Safe list reference
(!!*) :: [a] -> Int -> Maybe a
[] !!* _ = mzero
(x:xs) !!* n | n == 0    = return x
             | otherwise = xs !!* (n-1)
