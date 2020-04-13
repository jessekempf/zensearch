{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Zensearch.Types.OrganizationSpec (spec) where

import           Data.Aeson      (decodeStrict)
import           Data.ByteString (ByteString)
import           Test.Hspec
import           Text.Heredoc    (here)
import           Zensearch.Types (Organization (..), OrganizationID (..))


json :: ByteString
json = [here|
  {
    "_id": 101,
    "url": "http://initech.zendesk.com/api/v2/organizations/101.json",
    "external_id": "9270ed79-35eb-4a38-a46f-35725197ea8d",
    "name": "Enthaze",
    "domain_names": [
      "kage.com",
      "ecratic.com",
      "endipin.com",
      "zentix.com"
    ],
    "created_at": "2016-05-21T11:10:28 -10:00",
    "details": "MegaCorp",
    "shared_tickets": false,
    "tags": [
      "Fulton",
      "West",
      "Rodriguez",
      "Farley"
    ]
  }
|]

spec :: Spec
spec =
  describe "JSON decoder" $
    it "decodes the fields correctly" $ do
      let (Just decoded) = decodeStrict json

      decoded `shouldBe` Organization {
        orgId            = OrganizationID 101,
        orgUrl           = "http://initech.zendesk.com/api/v2/organizations/101.json",
        orgExternalId    = read "9270ed79-35eb-4a38-a46f-35725197ea8d",
        orgName          = "Enthaze",
        orgDomainNames   = ["kage.com", "ecratic.com", "endipin.com", "zentix.com"],
        orgCreatedAt     = read "2016-05-21 21:10:28",
        orgDetails       = Just "MegaCorp",
        orgSharedTickets = False,
        orgTags          = ["Fulton", "West", "Rodriguez", "Farley"]
      }
