{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Zensearch.Parsable (
  Parsable(..)
) where

import           Data.Attoparsec.Text (parseOnly)
import           Data.Attoparsec.Time (utcTime)
import           Data.Bifunctor       (first)
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text, pack, unpack)
import           Data.Time.Clock      (UTCTime)
import           Data.Typeable        (Typeable, typeRep)
import           Data.UUID            (UUID, fromText)
import           Text.Read            (readMaybe)


class Parsable a where
  parse :: Text -> Either Text a

instance Parsable a => Parsable (Maybe a) where
  parse "" = Right Nothing
  parse x  = parse x

instance {-# OVERLAPPABLE #-} forall a. (Read a, Typeable a) => Parsable a where
  parse str = case readMaybe $ unpack str of
                Nothing -> fail $ "Parsing error: '" <> unpack str <> "' is not a valid " <> show typ
                Just val -> return val
    where
      typ = typeRep (Proxy :: Proxy a)

instance Parsable Text where
  parse = Right

instance Parsable UUID where
  parse str = case fromText str of
                 Nothing -> Left $ "'" <> str <> "' is not a valid UUID"
                 Just x  -> Right x

instance Parsable UTCTime where
  parse = first pack . parseOnly utcTime
