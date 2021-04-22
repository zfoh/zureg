-- | Storing the registrants in a DynamoDB table.  Uses the `Eventful` library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Zureg.Database
    ( Config (..)
    , defaultConfig
    , Handle
    , withHandle
    , writeEvents
    , getRegistrant
    , getRegistrantUuids

    , putEmail
    , deleteEmail
    , lookupEmail

    , RegistrantsSummary (..)
    , lookupRegistrantsSummary
    , putRegistrantsSummary
    ) where

import           Control.Exception       (Exception, throwIO)
import           Control.Lens            (ix, (&), (.~), (^.), (^?))
import           Control.Monad           (forM, void, when)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Aeson              as A
import qualified Data.Aeson.TH.Extended  as A
import qualified Data.HashMap.Strict     as HMS
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as T
import qualified Eventful                as E
import qualified Eventful.Store.DynamoDB as E
import qualified Network.AWS.DynamoDB    as DynamoDB
import qualified Network.AWS.Extended    as Aws
import           Text.Read               (readMaybe)
import           Zureg.Model

data DatabaseException
    = WriterException E.EventWriteError
    | DecodeException String
    | NotFoundException String
    deriving (Show)

instance Exception DatabaseException

data Config = Config
    { cRegistrantTable :: !T.Text
    , cEmailTable      :: !T.Text
    , cSummariesTable  :: !T.Text
    }

$(A.deriveJSON A.options ''Config)

defaultConfig :: Config
defaultConfig = Config "registrants" "emails" "summaries"

data Handle = Handle
    { hConfig :: !Config
    , hAwsEnv :: !Aws.Env
    , hWriter :: !(E.EventStoreWriter Aws.AWS A.Value)
    , hReader :: !(E.VersionedEventStoreReader Aws.AWS A.Value)
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle hConfig@Config {..} f = do
    hAwsEnv <- Aws.smartEnv

    let hWriter = E.dynamoDBEventStoreWriter dynamoConfig
        hReader = E.dynamoDBEventStoreReader dynamoConfig

        dynamoConfig = E.defaultDynamoDBEventStoreConfig
            { E.dynamoDBEventStoreConfigTableName            = cRegistrantTable
            , E.dynamoDBEventStoreConfigUUIDAttributeName    = "uuid"
            , E.dynamoDBEventStoreConfigVersionAttributeName = "version"
            , E.dynamoDBEventStoreConfigEventAttributeName   = "event"
            }

    f Handle {..}

writeEvents :: A.ToJSON a => Handle -> E.UUID -> [Event a] -> IO ()
writeEvents Handle {..} uuid events = do
    mbError <- Aws.runResourceT $ Aws.runAWS hAwsEnv $
        E.storeEvents hWriter E.AnyVersion uuid $ map A.toJSON events
    maybe (return ()) (throwIO . WriterException) mbError


--------------------------------------------------------------------------------
-- Some utilities for working with DynamoDB

avs :: T.Text -> DynamoDB.AttributeValue
avs t = DynamoDB.attributeValue & DynamoDB.avS .~ Just t

avi :: Int -> DynamoDB.AttributeValue
avi n = DynamoDB.attributeValue & DynamoDB.avN .~ Just (T.pack $ show n)


getRegistrant :: A.FromJSON a => Handle -> E.UUID -> IO (Registrant a)
getRegistrant Handle {..} uuid = do
    values <- Aws.runResourceT $ Aws.runAWS hAwsEnv $
        E.getEvents hReader (E.allEvents uuid)
    events <- forM values $ \val ->
        case A.fromJSON (E.streamEventEvent val) of
            A.Error err -> throwIO $ DecodeException (show err)
            A.Success e -> return e

    when (null events) $ throwIO $ NotFoundException $
        "UUID " ++ show uuid ++ " not found"
    return $ E.latestProjection (registrantProjection uuid) events

-- | Perform a scan of the table to just return everything with version ID 0.
getRegistrantUuids :: Handle -> IO [E.UUID]
getRegistrantUuids Handle {..} = do
    values <- Aws.runResourceT $ Aws.runAWS hAwsEnv $ loop [] Nothing
    return values
  where
    loop acc mbLastKey | Just k <- mbLastKey, HMS.null k = return acc
    loop acc mbLastKey = do
        response <- Aws.send $ DynamoDB.scan (cRegistrantTable hConfig)
            & DynamoDB.sProjectionExpression .~ Just "#uuid"
            & DynamoDB.sFilterExpression .~ Just "version = :v"
            & DynamoDB.sExpressionAttributeValues .~ HMS.singleton ":v" zeroAv
            & DynamoDB.sExpressionAttributeNames .~ HMS.singleton "#uuid" "uuid"
            & (case mbLastKey of
                Nothing -> id
                Just k  -> DynamoDB.sExclusiveStartKey .~ k)

        uuids <- forM (response ^. DynamoDB.srsItems) $ \item -> maybe
            (liftIO $ throwIO $ DecodeException "Could not read uuid")
            return (itemUuid item)

        loop (uuids ++ acc) (Just $ response ^. DynamoDB.srsLastEvaluatedKey)

    zeroAv = DynamoDB.attributeValue & DynamoDB.avN .~ Just "0"

putEmail :: Handle -> T.Text -> E.UUID -> IO ()
putEmail Handle {..} email uuid = Aws.runResourceT $ Aws.runAWS hAwsEnv $
    void $ Aws.send $ DynamoDB.putItem (cEmailTable hConfig)
        & DynamoDB.piItem .~ HMS.fromList
            [ ("email", avs email)
            , ("uuid",  avs (E.uuidToText uuid))
            ]

deleteEmail :: Handle -> T.Text -> IO ()
deleteEmail Handle {..} email = Aws.runResourceT $ Aws.runAWS hAwsEnv $
    void $ Aws.send $ DynamoDB.deleteItem (cEmailTable hConfig)
        & DynamoDB.diKey .~ HMS.fromList [("email", avs email)]

lookupEmail :: Handle -> T.Text -> IO (Maybe E.UUID)
lookupEmail Handle {..} email = Aws.runResourceT $ Aws.runAWS hAwsEnv $ do
    response <- Aws.send $ DynamoDB.query (cEmailTable hConfig)
        & DynamoDB.qKeyConditionExpression .~ Just "email = :e"
        & DynamoDB.qExpressionAttributeValues .~ HMS.singleton ":e" (avs email)

    return $ itemUuid =<< listToMaybe (response ^. DynamoDB.qrsItems)

itemUuid :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe E.UUID
itemUuid item = do
    uuid <- HMS.lookup "uuid" item
    text <- uuid ^. DynamoDB.avS
    E.uuidFromText text

data RegistrantsSummary = RegistrantsSummary
    { rsTotal     :: Int
    , rsWaiting   :: Int
    , rsConfirmed :: Int
    , rsAttending :: Int
    , rsAvailable :: Int
    } deriving (Show)

$(A.deriveJSON A.options ''RegistrantsSummary)

registrantsSummaryToAttributeValue
    :: RegistrantsSummary -> DynamoDB.AttributeValue
registrantsSummaryToAttributeValue RegistrantsSummary {..} =
    DynamoDB.attributeValue & DynamoDB.avM .~ HMS.fromList
        [ ("total", avi rsTotal),
          ("waiting", avi rsWaiting ),
          ("confirmed", avi rsConfirmed),
          ("attending", avi rsAttending),
          ("available", avi rsAvailable)
        ]


registrantsSummaryFromAttributeValue
    :: DynamoDB.AttributeValue -> Maybe RegistrantsSummary
registrantsSummaryFromAttributeValue av = RegistrantsSummary
    <$> getInt "total" <*> getInt "waiting" <*> getInt "confirmed"
    <*> getInt "attending" <*> getInt "available"
  where
    getInt :: T.Text -> Maybe Int
    getInt key = do
        txt <- av ^? DynamoDB.avM . ix key . DynamoDB.avN . traverse
        readMaybe $ T.unpack txt

putRegistrantsSummary :: Handle -> RegistrantsSummary -> IO ()
putRegistrantsSummary Handle {..} summary =
    Aws.runResourceT $ Aws.runAWS hAwsEnv $ void $ Aws.send $
    DynamoDB.putItem (cSummariesTable hConfig)
        & DynamoDB.piItem .~ HMS.fromList
            [ ("name", avs "registrants")
            , ("summary", av)
            ]
  where
    av = registrantsSummaryToAttributeValue summary

lookupRegistrantsSummary :: Handle -> IO RegistrantsSummary
lookupRegistrantsSummary Handle {..} =
    Aws.runResourceT $ Aws.runAWS hAwsEnv $ do
        rsp <- Aws.send $ DynamoDB.getItem (cSummariesTable hConfig)
            & DynamoDB.giKey .~ HMS.fromList [("name", avs "registrants")]

        av <- maybe
            (liftIO $ throwIO $ NotFoundException "registrants summary") return
            (rsp ^? DynamoDB.girsItem . ix "summary")

        maybe
            (liftIO $ throwIO $ DecodeException "registrants summary") return
            (registrantsSummaryFromAttributeValue av)
