{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zensearch.Types.Common where

import           Data.Aeson      (FromJSON (..))
import           Numeric.Natural (Natural)

newtype UserID         = UserID         Natural deriving (Eq, Ord, Show, FromJSON)
newtype OrganizationID = OrganizationID Natural deriving (Eq, Ord, Show, FromJSON)
