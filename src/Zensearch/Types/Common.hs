{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Zensearch.Types.Common where

import           Data.Aeson          (FromJSON (..))
import           Numeric.Natural     (Natural)
import           Zensearch.Matchable (Matchable (..))
import           Zensearch.Parsable  (Parsable (..))


newtype UserID = UserID Natural deriving (Eq, Ord, Show, FromJSON)

instance Parsable UserID where
  parse str = UserID <$> parse str

instance Matchable UserID where
  type Comparand UserID = UserID

  match = (==)


newtype OrganizationID = OrganizationID Natural deriving (Eq, Ord, Show, FromJSON)

instance Parsable OrganizationID where
  parse str = OrganizationID <$> parse str

instance Matchable OrganizationID where
  type Comparand OrganizationID = OrganizationID

  match = (==)
