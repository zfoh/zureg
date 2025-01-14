{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Zureg.Database.Internal
    ( Config (..)
    , Handle (..)
    , withHandle
    , Transaction (..)
    , withTransaction
    ) where

import           Control.Exception          (bracket)
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as Pg

newtype Config = Config
    { cConnectionString :: T.Text
    } deriving (A.FromJSON, Show)

data Handle = Handle
    { hConfig :: !Config
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle hConfig f = do
    f Handle {..}

newtype Transaction = Transaction Pg.Connection

withTransaction :: Handle -> (Transaction -> IO a) -> IO a
withTransaction Handle {..} f = bracket
    (fmap Transaction $ Pg.connectPostgreSQL $
        T.encodeUtf8 $ cConnectionString hConfig)
    (\(Transaction conn) -> Pg.close conn)
    (\tx@(Transaction conn) -> Pg.withTransaction conn $ f tx)
