-- | Storing the registrants in a DynamoDB table.  Uses the `Eventful` library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Zureg.Database
    ( Config (..)
    , Handle
    , withHandle
    , writeEvents
    , getRegistrant
    , getRegistrantUuids
    , putEmail
    , deleteEmail
    , lookupEmail
    ) where

import           Control.Exception       (Exception, throwIO)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forM, void, when)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Aeson              as A
import qualified Data.Aeson.TH.Extended  as A
import qualified Data.HashMap.Strict     as HMS
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as T
import qualified Eventful                as E
import qualified Eventful.Store.DynamoDB as E
import qualified Network.AWS             as Aws
import qualified Network.AWS.DynamoDB    as DynamoDB
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
    }

data Handle = Handle
    { hConfig :: !Config
    , hAwsEnv :: !Aws.Env
    , hWriter :: !(E.EventStoreWriter Aws.AWS A.Value)
    , hReader :: !(E.VersionedEventStoreReader Aws.AWS A.Value)
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle hConfig@Config {..} f = do
    hAwsEnv <- Aws.newEnv Aws.Discover

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
            [ ("email", DynamoDB.attributeValue & DynamoDB.avS .~ Just email)
            , ("uuid",  DynamoDB.attributeValue &
                DynamoDB.avS .~ Just (E.uuidToText uuid))
            ]

deleteEmail :: Handle -> T.Text -> IO ()
deleteEmail Handle {..} email = Aws.runResourceT $ Aws.runAWS hAwsEnv $
    void $ Aws.send $ DynamoDB.deleteItem (cEmailTable hConfig)
        & DynamoDB.diKey .~ HMS.fromList 
        [ ("email", DynamoDB.attributeValue & DynamoDB.avS .~ Just email)]

lookupEmail :: Handle -> T.Text -> IO (Maybe E.UUID)
lookupEmail Handle {..} email = Aws.runResourceT $ Aws.runAWS hAwsEnv $ do
    response <- Aws.send $ DynamoDB.query (cEmailTable hConfig)
        & DynamoDB.qKeyConditionExpression .~ Just "email = :e"
        & DynamoDB.qExpressionAttributeValues .~ HMS.singleton ":e" emailAv

    return $ itemUuid =<< listToMaybe (response ^. DynamoDB.qrsItems)
  where
    emailAv = DynamoDB.attributeValue & DynamoDB.avS .~ Just email

itemUuid :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe E.UUID
itemUuid item = do
    uuid <- HMS.lookup "uuid" item
    text <- uuid ^. DynamoDB.avS
    E.uuidFromText text

$(A.deriveJSON A.options ''Config)
