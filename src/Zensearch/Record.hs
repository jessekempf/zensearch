{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Zensearch.Record (
  Field,
  field,
  fieldEq,
  fieldView,
  Record(..)
) where

import           Data.Proxy         (Proxy)
import           Data.Text          (Text, pack)
import           Zensearch.Parsable (Parsable (..))

{- A Field lets us operate on a record's field in a general but typesafe manner.
-- We hide the type of the field's value behind an existential so we can defer type-aware
-- operations to runtime instead of needing to know all types at compile time.
-- One consequence of this is that is is not possible to write a
-- `liftField :: Field t -> t -> (a -> b) -> b` which would take a field, and a Record, and
-- apply the (a -> b) function to the value pointed at by the field because we have no knowledge
-- of the type `a` at compile time.
-- If this is your first time reading Haskell, the saturated type of a `Field t` would look like
-- something like `Field User` or `Field Ticket`. This lets us ensure the `Field t` must be used
-- with a record of the correct type, but as you can see it tells us nothing about the value we
-- are referencing. That would be, in one hypothetical example, `Field User UserID`, but we can't
-- have a function that returns a `Field User UserID` _and_ a `Field User UUID`.
-}

data Field record = forall a. (Parsable a, Eq a, Show a) => Field (record -> a)


field :: forall record a. (Record record, Parsable a, Eq a, Show a) => (record -> a) -> Field record
field = Field

fieldEq :: Record record => Field record -> record -> Text -> Either Text Bool
fieldEq (Field getField) record other = do
  parsed <- parse other
  return $ parsed == getField record

fieldView :: Record record => Field record -> record -> Text
fieldView (Field getField) record = tshow $ getField record
  where
    tshow = pack . show

class Record record where
  project    :: Text -> Maybe (Field record)
  fieldnames :: Proxy record -> [Text]
