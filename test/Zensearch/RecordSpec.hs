{-# LANGUAGE OverloadedStrings #-}

module Zensearch.RecordSpec (spec) where

import           Test.Hspec
import           Zensearch.Record

newtype SimpleRecord = SimpleRecord {
  srInt  :: Int
}

instance Record SimpleRecord where
  project "int" = Just $ field srInt
  project _     = Nothing

  fieldnames _ = ["int"]

spec :: Spec
spec =
  describe "Field operations" $
    describe "fieldEq" $ do
      it "supports Int equality" $ do
        let sr              = SimpleRecord 2
            (Just intField) = project "int"

        fieldMatch intField sr "2" `shouldBe` Right True

      it "supports Int inequality" $ do
        let sr              = SimpleRecord 2
            (Just intField) = project "int"

        fieldMatch intField sr "3" `shouldBe` Right False
