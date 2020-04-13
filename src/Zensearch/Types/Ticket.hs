{-# LANGUAGE OverloadedStrings #-}

module Zensearch.Types.Ticket where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
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
