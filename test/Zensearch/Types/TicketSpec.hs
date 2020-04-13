{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Zensearch.Types.TicketSpec (spec) where

import           Data.Aeson      (decodeStrict)
import           Data.ByteString (ByteString)
import           Test.Hspec
import           Text.Heredoc    (here)
import           Zensearch.Types (OrganizationID (..), Ticket (..), UserID (..))


json :: ByteString
json = [here|
  {
    "_id": "436bf9b0-1147-4c0a-8439-6f79833bff5b",
    "url": "http://initech.zendesk.com/api/v2/tickets/436bf9b0-1147-4c0a-8439-6f79833bff5b.json",
    "external_id": "9210cdc9-4bee-485f-a078-35396cd74063",
    "created_at": "2016-04-28T11:19:34 -10:00",
    "type": "incident",
    "subject": "A Catastrophe in Korea (North)",
    "description": "Nostrud ad sit velit cupidatat laboris ipsum nisi amet laboris ex exercitation amet et proident. Ipsum fugiat aute dolore tempor nostrud velit ipsum.",
    "priority": "high",
    "status": "pending",
    "submitter_id": 38,
    "assignee_id": 24,
    "organization_id": 116,
    "tags": [
      "Ohio",
      "Pennsylvania",
      "American Samoa",
      "Northern Mariana Islands"
    ],
    "has_incidents": false,
    "due_at": "2016-07-31T02:37:50 -10:00",
    "via": "web"
  }
|]

spec :: Spec
spec =
  describe "JSON decoder" $
    it "decodes the fields correctly" $ do
      let (Just decoded) = decodeStrict json

      decoded `shouldBe` Ticket {
        ticketId             = read "436bf9b0-1147-4c0a-8439-6f79833bff5b",
        ticketUrl            = "http://initech.zendesk.com/api/v2/tickets/436bf9b0-1147-4c0a-8439-6f79833bff5b.json",
        ticketExternalId     = read "9210cdc9-4bee-485f-a078-35396cd74063",
        ticketCreatedAt      = read "2016-04-28 21:19:34",
        ticketType           = Just "incident",
        ticketSubject        = "A Catastrophe in Korea (North)",
        ticketDescription    = Just "Nostrud ad sit velit cupidatat laboris ipsum nisi amet laboris ex exercitation amet et proident. Ipsum fugiat aute dolore tempor nostrud velit ipsum.",
        ticketPriority       = "high",
        ticketStatus         = "pending",
        ticketSubmitterId    = UserID 38,
        ticketAssigneeId     = Just $ UserID 24,
        ticketOrganizationId = Just $ OrganizationID 116,
        ticketTags           = ["Ohio", "Pennsylvania", "American Samoa", "Northern Mariana Islands"],
        ticketHasIncidents   = False,
        ticketDueAt          = Just $ read "2016-07-31 12:37:50",
        ticketVia            = "web"
      }
