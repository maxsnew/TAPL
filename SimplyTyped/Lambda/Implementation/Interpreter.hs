module Interpreter where

import Grammar (Term(..))

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.Maybe (fromJust, isJust)

topShift :: Int -> Term -> Term
topShift i = shift i 0

shift :: Int -> Int -> Term -> Term
shift _ _ t | isVal t = t
shift inc cut (TIf t1 t2 t3) = TIf (go t1) (go t2) (go t3)
  where go = shift inc cut
shift inc cut v@(TVar i) | i < cut = v
                         | otherwise = TVar (i+inc)
shift inc cut (TAbs ty t)  = TAbs ty (shift inc (cut+1) t)
shift inc cut (TApp t1 t2) = TApp (shift inc cut t1)
                                  (shift inc cut t2)

subst :: Int -> Term -> Term -> Term
subst _ _ TTrue = TTrue
subst _ _ TFalse = TFalse
subst _ _ i@TNat{} = i
subst var sub (TIf t1 t2 t3) = TIf (go t1) (go t2) (go t3)
  where go = subst var sub

subst var sub t@(TVar i) | var == i  = sub
                         | otherwise = t
subst var sub (TAbs ty t)  = TAbs ty (subst (var+1) (shift 1 0 sub) t)
subst var sub (TApp t1 t2) = TApp (subst var sub t1)
                                  (subst var sub t2)

isVal :: Term -> Bool
isVal TTrue = True
isVal TFalse = True
isVal TNat{} = True
isVal TAbs{} = True
isVal _     = False

-- | Small step evaluator
eval1 :: Term -> Maybe Term
eval1 t | isVal t = mzero
eval1 TVar{} = mzero
eval1 (TIf TTrue t2 _) = return t2
eval1 (TIf TFalse _ t3) = return t3
eval1 (TIf t1 t2 t3) = TIf <$> eval1 t1 <*> pure t2 <*> pure t3
eval1 (TApp l@(TAbs _ t) v) | isVal v   = return $ termSubst v t
                            | otherwise = TApp l <$> eval1 v
eval1 (TApp t1 t2) = TApp <$> eval1 t1 <*> pure t2

evals :: Term -> [Term]
evals = map fromJust . takeWhile isJust . iterate (eval1 =<<) . Just

-- | Transitive Closure of eval1
eval :: Term -> Term
eval = last . evals

-- | Big step evaluator
eval' :: Term -> Maybe Term
eval' t | isVal t = return t
eval' (TIf t1 t2 t3) = do
  t1' <- eval' t1
  t2' <- eval' t2
  t3' <- eval' t3
  case t1' of
    TTrue  -> return t2'
    TFalse -> return t3'
    _      -> mzero
eval' (TApp t1 t2) = do
  t1' <- eval' t1
  t2' <- eval' t2
  case t1' of
    (TAbs _ t) -> eval' $ termSubst t2' t
    _       -> mzero
eval' TVar{} = mzero

termSubst :: Term -> Term -> Term
termSubst sub t = topShift (-1) (subst 0 (topShift 1 sub) t)

-- | Test
prop_big_small_agree :: Term -> Bool  
prop_big_small_agree t =
  (isJust bigT && isVal smallT && smallT == fromJust bigT) 
  || not (isJust bigT || isVal smallT)
  where smallT = eval t
        bigT   = eval' t
