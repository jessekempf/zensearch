{-# LANGUAGE OverloadedStrings #-}

module Zensearch.Types.User where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
import           Zensearch.Record       (Record (..), field)
import           Zensearch.Types.Common (OrganizationID, UserID)


data User = User {
  userId             :: UserID,
  userUrl            :: Text,
  userExternalId     :: UUID,
  userName           :: Text,
  userAlias          :: Maybe Text,
  userCreatedAt      :: UTCTime,
  userActive         :: Bool,
  userVerified       :: Maybe Bool,
  userShared         :: Bool,
  userLocale         :: Maybe Text,
  userTimezone       :: Maybe Text,
  userLastLoginAt    :: UTCTime,
  userEmail          :: Maybe Text,
  userPhone          :: Text,
  userSignature      :: Text,
  userOrganizationId :: Maybe OrganizationID,
  userTags           :: Set Text,
  userSuspended      :: Bool,
  userRole           :: Text
} deriving (Eq, Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
                User <$> obj .:  "_id"
                     <*> obj .:  "url"
                     <*> obj .:  "external_id"
                     <*> obj .:  "name"
                     <*> obj .:? "alias"
                     <*> obj .:  "created_at"
                     <*> obj .:  "active"
                     <*> obj .:? "verified"
                     <*> obj .:  "shared"
                     <*> obj .:? "locale"
                     <*> obj .:? "timezone"
                     <*> obj .:  "last_login_at"
                     <*> obj .:? "email"
                     <*> obj .:  "phone"
                     <*> obj .:  "signature"
                     <*> obj .:? "organization_id"
                     <*> obj .:  "tags"
                     <*> obj .:  "suspended"
                     <*> obj .:  "role"

instance Record User where
  project "_id"             = Just $ field userId
  project "url"             = Just $ field userUrl
  project "external_id"     = Just $ field userExternalId
  project "name"            = Just $ field userName
  project "alias"           = Just $ field userAlias
  project "created_at"      = Just $ field userCreatedAt
  project "active"          = Just $ field userActive
  project "verified"        = Just $ field userVerified
  project "shared"          = Just $ field userShared
  project "locale"          = Just $ field userLocale
  project "timezone"        = Just $ field userTimezone
  project "last_login_at"   = Just $ field userLastLoginAt
  project "email"           = Just $ field userEmail
  project "phone"           = Just $ field userPhone
  project "signature"       = Just $ field userSignature
  project "organization_id" = Just $ field userOrganizationId
  project "tags"            = Just $ field userTags
  project "suspended"       = Just $ field userSuspended
  project "role"            = Just $ field userRole
  project _                 = Nothing

  fieldnames _ = [
      "_id",
      "url",
      "external_id",
      "name",
      "alias",
      "created_at",
      "active",
      "verified",
      "shared",
      "locale",
      "timezone",
      "last_login_at",
      "email",
      "phone",
      "signature",
      "organization_id",
      "tags",
      "suspended",
      "role"
    ]
