{-# LANGUAGE OverloadedStrings #-}

module Zensearch.Types.Organization where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
import           Zensearch.Record       (Record (..), field)
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

instance Record Organization where
  project "_id"            = Just $ field orgId
  project "url"            = Just $ field orgUrl
  project "external_id"    = Just $ field orgExternalId
  project "name"           = Just $ field orgName
  project "domain_names"   = Just $ field orgDomainNames
  project "created_at"     = Just $ field orgCreatedAt
  project "details"        = Just $ field orgDetails
  project "shared_tickets" = Just $ field orgSharedTickets
  project "tags"           = Just $ field orgTags
  project _                = Nothing

  fieldnames _ = [
      "_id",
      "url",
      "external_id",
      "name",
      "domain_names",
      "created_at",
      "details",
      "shared_tickets",
      "tags"
    ]
