{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zensearch.Types.Common where

import           Data.Aeson         (FromJSON (..))
import           Numeric.Natural    (Natural)
import           Zensearch.Parsable (Parsable (..))

newtype UserID         = UserID         Natural deriving (Eq, Ord, Show, FromJSON)

instance Parsable UserID where
  parse str = UserID <$> parse str

newtype OrganizationID = OrganizationID Natural deriving (Eq, Ord, Show, FromJSON)

instance Parsable OrganizationID where
  parse str = OrganizationID <$> parse str
