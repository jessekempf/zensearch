{-# LANGUAGE OverloadedStrings #-}

module Zensearch.Types.User where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
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
