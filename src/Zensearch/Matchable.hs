{-# LANGUAGE TypeFamilies #-}

module Zensearch.Matchable (
  Matchable(..)
) where

import           Data.Set        (Set)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID       (UUID)

class Matchable a where
  type Comparand a :: *

  match :: a -> Comparand a -> Bool

instance Matchable Bool where
  type Comparand Bool = Bool

  match = (==)

instance Matchable Int where
  type Comparand Int = Int

  match = (==)

instance Matchable Text where
  type Comparand Text = Text

  match = (==)

instance Matchable UUID where
  type Comparand UUID = UUID

  match = (==)

instance Matchable UTCTime where
  type Comparand UTCTime = UTCTime

  match = (==)

instance Eq a => Matchable (Maybe a) where
  type Comparand (Maybe a) = Maybe a

  match = (==)

instance Eq a => Matchable (Set a) where
  type Comparand (Set a) = a

  match = flip elem
