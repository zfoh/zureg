{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Zureg.Database.Internal
    ( Config (..)
    , Handle (..)
    , withHandle
    , Transaction (..)
    , withTransaction
    ) where

import           Control.Exception          (bracket)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.TH.Extended     as A
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as Pg

data Config = Config
    { configConnectionString :: !T.Text
    , configMigrations       :: !FilePath
    } deriving (Show)

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
        T.encodeUtf8 $ configConnectionString hConfig)
    (\(Transaction conn) -> Pg.close conn)
    (\tx@(Transaction conn) -> Pg.withTransaction conn $ f tx)

$(A.deriveJSON A.options ''Config)
