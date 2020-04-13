{-# LANGUAGE OverloadedStrings #-}

module Zensearch.Types.Organization where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
import           Zensearch.Types.Common (OrganizationID)


data Organization = Organization {
  orgId            :: OrganizationID,
  orgUrl           :: Text,
  orgExternalId    :: UUID,
  orgName          :: Text,
  orgDomainNames   :: Set Text,
  orgCreatedAt     :: UTCTime,
  orgDetails       :: Maybe Text,
  orgSharedTickets :: Bool,
  orgTags          :: Set Text
} deriving (Eq, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
                Organization <$> obj .:  "_id"
                             <*> obj .:  "url"
                             <*> obj .:  "external_id"
                             <*> obj .:  "name"
                             <*> obj .:  "domain_names"
                             <*> obj .:  "created_at"
                             <*> obj .:? "details"
                             <*> obj .:  "shared_tickets"
                             <*> obj .:  "tags"
