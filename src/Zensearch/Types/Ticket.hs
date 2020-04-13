{-# LANGUAGE OverloadedStrings #-}

module Zensearch.Types.Ticket where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
import           Zensearch.Record       (Record (..), field)
import           Zensearch.Types.Common (OrganizationID, UserID)


data Ticket = Ticket {
  ticketId             :: UUID,
  ticketUrl            :: Text,
  ticketExternalId     :: UUID,
  ticketCreatedAt      :: UTCTime,
  ticketType           :: Maybe Text,
  ticketSubject        :: Text,
  ticketDescription    :: Maybe Text,
  ticketPriority       :: Text,
  ticketStatus         :: Text,
  ticketSubmitterId    :: UserID,
  ticketAssigneeId     :: Maybe UserID,
  ticketOrganizationId :: Maybe OrganizationID,
  ticketTags           :: Set Text,
  ticketHasIncidents   :: Bool,
  ticketDueAt          :: Maybe UTCTime,
  ticketVia            :: Text
} deriving (Eq, Show)

instance FromJSON Ticket where
  parseJSON = withObject "Ticket" $ \obj ->
                Ticket <$> obj .:  "_id"
                       <*> obj .:  "url"
                       <*> obj .:  "external_id"
                       <*> obj .:  "created_at"
                       <*> obj .:? "type"
                       <*> obj .:  "subject"
                       <*> obj .:? "description"
                       <*> obj .:  "priority"
                       <*> obj .:  "status"
                       <*> obj .:  "submitter_id"
                       <*> obj .:? "assignee_id"
                       <*> obj .:? "organization_id"
                       <*> obj .:  "tags"
                       <*> obj .:  "has_incidents"
                       <*> obj .:? "due_at"
                       <*> obj .:  "via"

instance Record Ticket where
  project "_id"             = Just $ field ticketId
  project "url"             = Just $ field ticketUrl
  project "external_id"     = Just $ field ticketExternalId
  project "created_at"      = Just $ field ticketCreatedAt
  project "type"            = Just $ field ticketType
  project "subject"         = Just $ field ticketSubject
  project "description"     = Just $ field ticketDescription
  project "priority"        = Just $ field ticketPriority
  project "status"          = Just $ field ticketStatus
  project "submitter_id"    = Just $ field ticketSubmitterId
  project "assignee_id"     = Just $ field ticketAssigneeId
  project "organization_id" = Just $ field ticketOrganizationId
  project "tags"            = Just $ field ticketTags
  project "has_incidents"   = Just $ field ticketHasIncidents
  project "due_at"          = Just $ field ticketDueAt
  project "via"             = Just $ field ticketVia
  project _                 = Nothing

  fieldnames _ = [
      "_id",
      "url",
      "external_id",
      "created_at",
      "type",
      "subject",
      "description",
      "priority",
      "status",
      "submitter_id",
      "assignee_id",
      "organization_id",
      "tags",
      "has_incidents",
      "due_at",
      "via"
    ]
