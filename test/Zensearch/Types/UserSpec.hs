{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Zensearch.Types.UserSpec (spec) where

import           Data.Aeson      (decodeStrict)
import           Data.ByteString (ByteString)
import           Test.Hspec
import           Text.Heredoc    (here)
import           Zensearch.Types (OrganizationID (..), User (..), UserID (..))


json :: ByteString
json = [here|
  {
    "_id": 1,
    "url": "http://initech.zendesk.com/api/v2/users/1.json",
    "external_id": "74341f74-9c79-49d5-9611-87ef9b6eb75f",
    "name": "Francisca Rasmussen",
    "alias": "Miss Coffey",
    "created_at": "2016-04-15T05:19:46 -10:00",
    "active": true,
    "verified": true,
    "shared": false,
    "locale": "en-AU",
    "timezone": "Sri Lanka",
    "last_login_at": "2013-08-04T01:03:27 -10:00",
    "email": "coffeyrasmussen@flotonic.com",
    "phone": "8335-422-718",
    "signature": "Don't Worry Be Happy!",
    "organization_id": 119,
    "tags": [
      "Springville",
      "Sutton",
      "Hartsville/Hartley",
      "Diaperville"
    ],
    "suspended": true,
    "role": "admin"
  }
|]

spec :: Spec
spec =
  describe "JSON decoder" $
    it "decodes the fields correctly" $ do
      let (Just decoded) = decodeStrict json

      decoded `shouldBe` User {
        userId             = UserID 1,
        userUrl            = "http://initech.zendesk.com/api/v2/users/1.json",
        userExternalId     = read "74341f74-9c79-49d5-9611-87ef9b6eb75f",
        userName           = "Francisca Rasmussen",
        userAlias          = Just "Miss Coffey",
        userCreatedAt      = read "2016-04-15 15:19:46",
        userActive         = True,
        userVerified       = Just True,
        userShared         = False,
        userLocale         = Just "en-AU",
        userTimezone       = Just "Sri Lanka",
        userLastLoginAt    = read "2013-08-04 11:03:27",
        userEmail          = Just "coffeyrasmussen@flotonic.com",
        userPhone          = "8335-422-718",
        userSignature      = "Don't Worry Be Happy!",
        userOrganizationId = Just $ OrganizationID 119,
        userTags           = ["Springville", "Sutton", "Hartsville/Hartley", "Diaperville"],
        userSuspended      = True,
        userRole           = "admin"
      }
