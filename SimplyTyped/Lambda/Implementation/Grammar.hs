module Grammar
       where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set as Set (Set, empty, singleton, toList, union, unions)
import Test.QuickCheck (Arbitrary(..), elements, oneof, sized)

-- | Simply Typed Lambda Calculus
data Type = TyBool
          | TyArr Type Type
          deriving (Show, Eq)

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TVar Int
          | TAbs Type Term
          | TApp Term Term
          deriving (Show, Eq)

-- | Untyped Lambda Calculus using explicit variable names.
data NamedTerm = NTrue
               | NFalse
               | NIf NamedTerm NamedTerm NamedTerm
               | NVar String
               | NAbs Type String NamedTerm
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
            NTrue -> return TTrue
            NFalse -> return TFalse
            NIf t1 t2 t3 -> TIf <$> rec t1 <*> rec t2 <*> rec t3
            (NVar s)     -> let i = fromJust $ s `elemIndex` ctx in 
              return $ TVar i
            (NAbs ty s bod) ->
              TAbs ty <$> (local (s:) $ rec bod)
            (NApp t1 t2) ->
              TApp <$> rec t1 <*> rec t2

freeVars :: NamedTerm -> [String]
freeVars t = Set.toList $ runReader (go t) []
  where go :: NamedTerm -> Reader [String] (Set String)
        go nt = do
          bound <- ask
          case nt of
            NTrue      -> return Set.empty
            NFalse     -> return Set.empty
            NIf nt1 nt2 nt3 -> do
              free1 <- go nt1
              free2 <- go nt2
              free3 <- go nt3
              return $ Set.unions $ [free1, free2, free3]
            NVar s     -> return $ if s `elem` bound
                                     then Set.empty
                                     else Set.singleton s
            NAbs _ s bod -> local (s:) $ go bod
            NApp t1 t2 -> do
              free1 <- go t1
              free2 <- go t2
              return $ free1 `Set.union` free2
            
restoreNames :: Term -> [String] -> NamedTerm
restoreNames t' ctx = runReader (rec t') (nameCtx)
  where rec :: Term -> Reader ([String], [String]) NamedTerm
        rec TTrue          = return NTrue
        rec TFalse         = return NFalse
        rec (TIf t1 t2 t3) = NIf <$> rec t1 <*> rec t2 <*> rec t3
        rec (TVar i)     = do
          (names, _) <- ask
          return $ NVar (names !! i)
        rec (TApp t1 t2) = NApp <$> rec t1 <*> rec t2
        rec (TAbs ty t)     = do
          (_, unused) <- ask
          let s = head unused in
            NAbs ty s <$> (local ((s:) *** tail) $ rec t)
        nameCtx = splitAt (length ctx) validIdentifiers
-- | Testing
instance Arbitrary Type where
  arbitrary = sized type'
    where type' 0 = return TyBool
          type' n = TyArr <$> halved <*> halved
            where halved = type' (n `div` 2)
  shrink (TyArr t1 t2) = [t1, t2]
  shrink TyBool        = []

instance Arbitrary Term where
  arbitrary = sized term'
    where term' 0 = oneof $ (TVar . abs <$> arbitrary) : map return [TTrue, TFalse]
          term' n = oneof [ TIf <$> thirded <*> thirded <*> thirded
                          , TAbs <$> arbitrary <*> (term' (n-1))
                          , TApp <$> halved <*> halved
                          ]
            where halved = term' (n `div` 2)
                  thirded = term' (n `div` 3)
  shrink (TIf t1 t2 t3) = [t1, t2, t3]
  shrink (TApp t1 t2) = [t1, t2]
  shrink (TAbs ty t) = t : ((flip TAbs t) <$> shrink ty)
  shrink _ = []

instance Arbitrary NamedTerm where
  arbitrary = sized term'
    where term' 0 = oneof $ (NVar <$> vars) : map return [NTrue, NFalse]
          term' n = oneof [ NIf <$> thirded <*> thirded <*> thirded
                          , NAbs <$> arbitrary <*> vars <*> (term' (n-1))
                          , NApp <$> halved <*> halved
                          ]
            where halved = term' (n `div` 2)
                  thirded = term' (n `div` 3)
          vars = elements . fmap (:[]) $ ['a'..'z']
  shrink (NApp t1 t2) = [t1, t2]
  shrink (NAbs _ _ t) = [t]
  shrink _ = []

prop_idempotent_encoding :: NamedTerm -> Bool
prop_idempotent_encoding nt = run nt == (run . run $ nt)
  where run = uncurry restoreNames . removeNames
