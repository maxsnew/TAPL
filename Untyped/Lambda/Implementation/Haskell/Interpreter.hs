module Interpreter where

import Grammar (Term(..))

import Control.Monad (mzero)
import Data.Functor ((<$>))
import Data.Maybe (fromJust, isJust)

topShift :: Int -> Term -> Term
topShift i t = shift i 0 t

shift :: Int -> Int -> Term -> Term
shift inc cut v@(Var i) | i < cut = v
                        | otherwise = Var (i+inc)
shift inc cut (Abs t)     = Abs (shift inc (cut+1) t)
shift inc cut (App t1 t2) = App (shift inc cut t1)
                                (shift inc cut t2)

subst :: Int -> Term -> Term -> Term
subst var sub t@(Var i) | var == i  = sub
                        | otherwise = t
subst var sub (Abs t)     = Abs (subst (var+1) (shift 1 0 sub) t)
subst var sub (App t1 t2) = App (subst var sub t1)
                                (subst var sub t2)

isVal :: Term -> Bool
isVal Abs{} = True
isVal _     = False

-- | Small step evaluator
eval1 :: Term -> Maybe Term
eval1 (App l@(Abs t) v) | isVal v   = return $ termSubst v t
                        | otherwise = App l <$> eval1 v
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 _           = mzero

evals :: Term -> [Term]
evals = map fromJust . takeWhile isJust . iterate (eval1 =<<) . Just

-- | Transitive Closure of eval1
eval :: Term -> Term
eval = last . evals

-- | Big step evaluator
eval' :: Term -> Maybe Term
eval' (App t1 t2) = do
  t1' <- eval' t1
  t2' <- eval' t2
  case t1' of
    (Abs t) -> eval' $ termSubst t2' t
    _       -> mzero
eval' l@Abs{} = return l
eval' _       = mzero

termSubst :: Term -> Term -> Term
termSubst sub t = topShift (-1) (subst 0 (topShift 1 sub) t)

-- | Test
prop_big_small_agree :: Term -> Bool  
prop_big_small_agree t =
  (isJust bigT && isVal smallT && smallT == (fromJust bigT)) 
  || not ((isJust bigT) || (isVal smallT))
  where smallT = eval t
        bigT   = eval' t
