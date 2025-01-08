-- | Storing the registrants in a DynamoDB table.  Uses the `Eventful` library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Zureg.Database
    ( Config (..)
    , configFromEnv
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

import           Control.Exception      (Exception)
import qualified Data.Aeson             as A
import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import           Data.UUID              (UUID)
import           System.Environment     (lookupEnv)
import           Zureg.Model

data DatabaseException
    = WriterException
    | DecodeException String
    | NotFoundException String
    deriving (Show)

instance Exception DatabaseException

data Config = Config
    { cConnectionString :: !T.Text
    }

configFromEnv :: IO Config
configFromEnv = do
    cstring <- lookupEnv "ZUREG_DB" >>= maybe (fail "ZUREG_DB not set") pure
    pure Config {cConnectionString = T.pack cstring}

data Handle = Handle
    { hConfig :: !Config
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle hConfig f = do
    f Handle {..}

writeEvents :: A.ToJSON a => Handle -> UUID -> [Event a] -> IO ()
writeEvents _ _ _ = pure ()

getRegistrant :: A.FromJSON a => Handle -> UUID -> IO (Registrant a)
getRegistrant _ _ = undefined

getRegistrantUuids :: Handle -> IO [UUID]
getRegistrantUuids _ = pure []

putEmail :: Handle -> T.Text -> UUID -> IO ()
putEmail _ _ _ = pure ()

deleteEmail :: Handle -> T.Text -> IO ()
deleteEmail _ _ = pure ()

lookupEmail :: Handle -> T.Text -> IO (Maybe UUID)
lookupEmail _ _ = pure Nothing

data RegistrantsSummary = RegistrantsSummary
    { rsTotal     :: Int
    , rsWaiting   :: Int
    , rsConfirmed :: Int
    , rsAttending :: Int
    , rsAvailable :: Int
    , rsScanned   :: Int
    , rsSpam      :: Int
    } deriving (Show)

$(A.deriveJSON A.options ''RegistrantsSummary)

putRegistrantsSummary :: Handle -> RegistrantsSummary -> IO ()
putRegistrantsSummary _ _ = pure ()

lookupRegistrantsSummary :: Handle -> IO RegistrantsSummary
lookupRegistrantsSummary _ = undefined
