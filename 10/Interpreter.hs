module Interpreter where

import Grammar (Term(..))

import Control.Monad (mzero)
import Data.Functor ((<$>))
import Data.Maybe (fromJust, isJust)

topShift :: Int -> Term -> Term
topShift i = shift i 0

shift :: Int -> Int -> Term -> Term
shift _ _ TTrue = TTrue
shift _ _ TFalse = TFalse
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
isVal TAbs{} = True
isVal _     = False

-- | Small step evaluator
eval1 :: Term -> Maybe Term
eval1 (TApp l@(TAbs _ t) v) | isVal v   = return $ termSubst v t
                        | otherwise = TApp l <$> eval1 v
eval1 (TApp t1 t2) = flip TApp t2 <$> eval1 t1
eval1 _           = mzero

evals :: Term -> [Term]
evals = map fromJust . takeWhile isJust . iterate (eval1 =<<) . Just

-- | Transitive Closure of eval1
eval :: Term -> Term
eval = last . evals

-- | Big step evaluator
eval' :: Term -> Maybe Term
eval' (TApp t1 t2) = do
  t1' <- eval' t1
  t2' <- eval' t2
  case t1' of
    (TAbs _ t) -> eval' $ termSubst t2' t
    _       -> mzero
eval' l@TAbs{} = return l
eval' _       = mzero

termSubst :: Term -> Term -> Term
termSubst sub t = topShift (-1) (subst 0 (topShift 1 sub) t)

-- | Test
prop_big_small_agree :: Term -> Bool  
prop_big_small_agree t =
  (isJust bigT && isVal smallT && smallT == fromJust bigT) 
  || not (isJust bigT || isVal smallT)
  where smallT = eval t
        bigT   = eval' t
