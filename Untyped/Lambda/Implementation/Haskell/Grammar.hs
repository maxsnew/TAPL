module Grammar where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set as Set (Set, empty, singleton, toList, union)
import Test.QuickCheck (Arbitrary(..), elements, oneof, sized)

-- | Untyped Lambda Calculus using De Bruijn indices for variables.
data Term = Var Int
          | Abs Term
          | App Term Term
          deriving (Show, Eq)

-- | Untyped Lambda Calculus using explicit variable names.
data NamedTerm = NVar String
               | NAbs String NamedTerm
               | NApp NamedTerm NamedTerm
               deriving (Show, Eq)

validIdentifiers :: [String]
validIdentifiers = (flip (:)) <$> ("":validRest) <*> validStart
  where validStart = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
        validRest = (flip (:)) <$> ("" : validRest) <*> (validStart ++ ['0'..'9'] ++ "'")

-- | Convert back and forth between De Bruijn and explicit names
-- | Returns the nameless term and a list representing the encodings of
-- | the free variables (the naming context).
removeNames :: NamedTerm -> (Term, [String])
removeNames t = withFree $ runReader (rec t) free
  where withFree x = (x, free)
        free = freeVars t
        rec nt = do
          ctx <- ask
          case nt of
            (NVar s)     -> let i = fromJust $ s `elemIndex` ctx in 
              return $ Var i
            (NAbs s bod) ->
              Abs <$> (local (s:) $ rec bod)
            (NApp t1 t2) ->
              App <$> rec t1 <*> rec t2

freeVars :: NamedTerm -> [String]
freeVars t = Set.toList $ runReader (go t) []
  where go :: NamedTerm -> Reader [String] (Set String)
        go nt = do
          bound <- ask
          case nt of
            NVar s     -> return $ if s `elem` bound
                                     then Set.empty
                                     else Set.singleton s
            NAbs s bod -> local (s:) $ go bod
            NApp t1 t2 -> do
              free1 <- go t1
              free2 <- go t2
              return $ free1 `Set.union` free2
            
restoreNames :: Term -> [String] -> NamedTerm
restoreNames t' ctx = runReader (rec t') (nameCtx)
  where rec :: Term -> Reader ([String], [String]) NamedTerm
        rec (Var i)     = do
          (names, _) <- ask
          return $ NVar (names !! i)
        rec (App t1 t2) = NApp <$> rec t1 <*> rec t2
        rec (Abs t)     = do
          (_, unused) <- ask
          let s = head unused in
            NAbs s <$> (local ((s:) *** tail) $ rec t)
        nameCtx = splitAt (length ctx) validIdentifiers
-- | Testing
instance Arbitrary Term where
  arbitrary = sized term'
    where term' 0 = Var . abs <$> arbitrary
          term' n = oneof [ Abs <$> (term' (n-1))
                          , App <$> halved <*> halved
                          ]
            where halved = term' (n `div` 2)

  shrink (App t1 t2) = [t1, t2]
  shrink (Abs t) = [t]
  shrink _ = []

instance Arbitrary NamedTerm where
  arbitrary = sized term'
    where term' 0 = NVar <$> vars
          term' n = oneof [ NAbs <$> vars <*> (term' (n-1))
                          , NApp <$> halved <*> halved
                          ]
            where halved = term' (n `div` 2)
          vars = elements . fmap (:[]) $ ['a'..'z']
  shrink (NApp t1 t2) = [t1, t2]
  shrink (NAbs _ t) = [t]
  shrink _ = []

prop_idempotent_encoding :: NamedTerm -> Bool
prop_idempotent_encoding nt = run nt == (run . run $ nt)
  where run = uncurry restoreNames . removeNames
