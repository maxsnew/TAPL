module Grammar where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (Reader)
import Test.QuickCheck

data Term = Var Int
          | Abs Term
          | App Term Term
          deriving (Show, Eq)

data NamedTerm = NVar String
               | NAbs String NamedTerm
               | NApp NamedTerm NamedTerm
               deriving (Show, Eq)

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
          vars = elements . map (:[]) $ ['a'..'z']                  

  shrink (NApp t1 t2) = [t1, t2]
  shrink (NAbs _ t) = [t]
  shrink _ = []

-- | Named -> De Bruijn and back
removeNames :: NamedTerm -> Reader [String] Term
removeNames = undefined

restoreNames :: Term -> Reader [String] NamedTerm
restoreNames = undefined
