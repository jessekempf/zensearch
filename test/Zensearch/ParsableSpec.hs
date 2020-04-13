{-# LANGUAGE OverloadedStrings #-}

module Zensearch.ParsableSpec (spec) where

import           Data.Text          (Text)
import           Data.Time.Clock    (UTCTime)
import           Data.UUID          (UUID)
import           Numeric.Natural    (Natural)
import           Test.Hspec
import           Zensearch.Parsable


spec :: Spec
spec = do
  it "parses Ints correctly" $
    parse "3" `shouldBe` Right (3 :: Int)

  it "parses Nats correctly" $
    parse "3" `shouldBe` Right (3 :: Natural)

  it "parses Text correctly" $
    parse "foo" `shouldBe` Right ("foo" :: Text)

  it "parses UUIDs correctly" $
    parse "50f3fdbd-f8a6-481d-9bf7-572972856628" `shouldBe` Right (read "50f3fdbd-f8a6-481d-9bf7-572972856628" :: UUID)

  it "parses UTCTimes correctly" $
    parse "2016-05-21T11:10:28 -10:00" `shouldBe` Right (read "2016-05-21 21:10:28" :: UTCTime)
