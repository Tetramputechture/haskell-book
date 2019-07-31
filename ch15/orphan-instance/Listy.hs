module Listy where

import Data.Monoid

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

instance Monoid (Listy a) where
  mempty = Listy []

instance Semigroup (Listy a) where
  (Listy a) <> (Listy a') = Listy $ a <> a'
