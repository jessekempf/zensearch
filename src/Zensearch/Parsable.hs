{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Zensearch.Parsable (
  Parsable(..)
) where

import           Data.Attoparsec.Text (parseOnly)
import           Data.Attoparsec.Time (utcTime)
import           Data.Bifunctor       (first)
import           Data.Text            (Text, pack, unpack)
import           Data.Time.Clock      (UTCTime)
import           Data.UUID            (UUID, fromText)
import           Text.Read            (readEither)


class Parsable a where
  parse :: Text -> Either Text a

instance Parsable a => Parsable (Maybe a) where
  parse "" = Right Nothing
  parse x  = parse x

instance {-# OVERLAPPABLE #-} Read a => Parsable a where
  parse = first pack . readEither . unpack

instance Parsable Text where
  parse = Right

instance Parsable UUID where
  parse str = case fromText str of
                 Nothing -> Left $ "'" <> str <> "' is not a valid UUID"
                 Just x  -> Right x

instance Parsable UTCTime where
  parse = first pack . parseOnly utcTime
