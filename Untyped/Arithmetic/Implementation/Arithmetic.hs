module Arithmetic where

import Data.Maybe (fromJust, isJust)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Test.QuickCheck (Arbitrary(..), sized, elements, oneof)

{-
  Haskell translation of the OCaml code. 
  See 3/Numbers.rkt for the Redex grammar
-}

-- | t in our grammar.
data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TZero
          | TSucc Term
          | TPred Term
          | TIsZero Term
          deriving (Show, Eq)

-- | nv in our Grammar
isNumeric :: Term -> Bool
isNumeric TZero   = True
isNumeric (TSucc t) = isNumeric t
isNumeric _           = False

-- | v in our Grammar
isVal :: Term -> Bool
isVal TTrue           = True
isVal TFalse          = True
isVal t | isNumeric t = True
        | otherwise   = False

-- | Use Maybe instead of Exception (as mentioned in the book).
eval1 :: Term -> Maybe Term
eval1 (TIf TTrue{} t2 _)   = return t2
eval1 (TIf TFalse{} _ t3)  = return t3
eval1 (TIf t1 t2 t3)       = t1 `chain` (\t1' -> TIf t1' t2 t3)
eval1 (TSucc t1)           = t1 `chain` TSucc
eval1 (TPred TZero)        = return TZero
eval1 (TPred (TSucc nv))   | isNumeric nv = return nv
eval1 (TPred t)            = t `chain` TPred
eval1 (TIsZero TZero)      = return TTrue
eval1 (TIsZero (TSucc nv)) | isNumeric nv = return TFalse
eval1 (TIsZero t)          = t `chain` TIsZero
eval1 _                    = Nothing

evals :: Term -> [Term]
evals = map fromJust . takeWhile isJust . iterate (eval1 =<<) . Just

-- | Transitive Closure of eval1
eval :: Term -> Term
eval = last . evals

-- | Bigstep evaluator (Exercise 4.2.2)
eval' :: Term -> Maybe Term
eval' v | isVal v    = Just v
eval' (TIf t1 t2 t3) = do
  t1' <- eval' t1
  case t1' of
    TTrue  -> eval' t2
    TFalse -> eval' t3
    _      -> mzero
eval' (TSucc t) = do
  nv <- eval' t
  if isNumeric nv
    then TSucc <$> (eval' nv)
    else mzero
eval' (TPred t) = do
  t' <- eval' t
  case t' of
    TZero      -> return TZero
    TSucc nv | isNumeric nv -> return nv
    _          -> mzero
eval' (TIsZero t) = do
  t' <- eval' t
  case t' of
    TZero      -> return TTrue
    (TSucc nv) | isNumeric nv -> return TFalse
    _          -> mzero

chain :: Term -> (Term -> b) -> Maybe b
chain t f = eval1 t >>= (return . f)

instance Arbitrary Term where
  arbitrary = sized term'
    where term' 0 = elements [TTrue, TFalse, TZero]
          term' n = oneof [ TIf <$> third <*> third <*> third
                          , TSucc <$> less1
                          , TPred <$> less1
                          , TIsZero <$> less1
                          ]
            where third = term' $ n `div` 3
                  less1 = term' $ n-1
  shrink (TIf t1 t2 t3) = [TIf t1' t2' t3' | t1' <- shrink t1
                                           , t2' <- shrink t2
                                           , t3' <- shrink t3]
  shrink (TSucc t)   = TSucc <$> (shrink t)
  shrink (TPred t)   = TPred <$> (shrink t)
  shrink (TIsZero t) = TIsZero <$> (shrink t)
  shrink _           = []

{- | Check Ex 3.5.17
     Caught 4 bugs!!

     Property is that the bigstep and small semantics are the same.
     That is, either they both reduce to the same value or neither reduces 
     to a value.
-}
prop_big_small_agree :: Term -> Bool  
prop_big_small_agree t =
  (isJust bigT && isVal smallT && smallT == (fromJust bigT)) 
  || not ((isJust bigT) || (isVal smallT))
  where smallT = eval t
        bigT   = eval' t
