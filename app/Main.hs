{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Monad.Fail  (MonadFail)
import           Data.Aeson          (FromJSON, eitherDecodeFileStrict')
import           Data.Text           (Text, pack, unpack)
import           Options.Applicative
import           Text.Show.Pretty    (pPrint)
import           Zensearch.Record    (Field, Record, fieldMatch, fieldnames,
                                      project)
import qualified Zensearch.Types     as Types


{-
-- Data model
-}
newtype FieldName  = FieldName  Text deriving Show
newtype FieldValue = FieldValue Text deriving Show

data Table   = Organization | Ticket | User deriving (Read, Show)

data Command = Enumerate | List Table | Match Table FieldName FieldValue deriving Show

{-
-- Argument parsing
--}
parseArguments :: Parser Command
parseArguments = enumerate <|> list <|> match
  where
    table     = argument auto (metavar "TABLE")
    field     = FieldName <$> strArgument (metavar "FIELD")
    against   = FieldValue <$> strArgument (metavar "VALUE")

    enumArgs  = pure Enumerate
    listArgs  = List <$> table
    matchArgs = Match <$> table <*> field <*> against

    enumerate = subparser $ command "enumerate" $ info enumArgs  $ progDesc "Enumerate known tables"
    list      = subparser $ command "list"      $ info listArgs  $ progDesc "List matchable fields of a table"
    match     = subparser $ command "match"     $ info matchArgs $ progDesc "Find all matching records for a table"


{-
-- Helper functions
-}
tableSearch :: forall record m. (MonadFail m, Record record) => FieldName -> FieldValue -> [record] -> m [record]
tableSearch (FieldName name) (FieldValue val) table =
  case project name of
    Nothing                 -> fail $ unpack name <> " is not a valid field name"
    Just (field :: Field t) ->
      case search field table of
        Left err      -> fail $ unpack err
        Right matches -> return matches

  where
    search field recs = reverse <$> search' [] recs
      where
        search' accum []       = Right accum
        search' accum (r : rs) = case fieldMatch field r val of
                                          Left err -> Left err
                                          Right True  -> search' (r : accum) rs
                                          Right False -> search' accum rs


loadData :: forall record. FromJSON record => Table -> IO (Either String [record])
loadData Organization = eitherDecodeFileStrict' "organizations.json"
loadData Ticket       = eitherDecodeFileStrict' "tickets.json"
loadData User         = eitherDecodeFileStrict' "users.json"


validate :: MonadFail m => Either String a -> m a
validate (Left err)  = fail err
validate (Right val) = pure val


{-
-- Glue
-}
main :: IO ()
main =
  customExecParser pref opts >>= \case
    Enumerate            -> doEnumerate
    List table           -> doList table
    Match table name val -> doMatch table name val

  where
    pref = prefs (showHelpOnEmpty <> showHelpOnError)
    opts = info (parseArguments <**> helper) fullDesc

    printList = mapM_ (\x -> putStr "  - " >> print x)

    tshow = pack . show

    doEnumerate = do
      putStrLn  "Known tables:"
      printList $ map tshow [Organization, Ticket, User]

    doList table = do
      putStrLn $ "Searchable fields for " <> show table <> ":"
      printList $ case table of
        Organization -> fieldnames @Types.Organization
        Ticket       -> fieldnames @Types.Ticket
        User         -> fieldnames @Types.User

    doMatch Organization = doMatch' @Types.Organization Organization
    doMatch Ticket       = doMatch' @Types.Ticket Ticket
    doMatch User         = doMatch' @Types.User User

    doMatch' :: forall record. (Record record, FromJSON record, Show record) => Table -> FieldName -> FieldValue -> IO ()
    doMatch' tableKind fn fv = do
      matches <- tableSearch fn fv =<< validate =<< loadData @record tableKind

      case matches of
        [] -> putStrLn "No results found"
        ms -> pPrint ms
