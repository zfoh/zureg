{-# LANGUAGE TemplateHaskell #-}
module Zureg.AWS
    ( Config
    , smartEnv
    ) where

import           Amazonka
import           Amazonka.Auth
import qualified Amazonka.Data          as Amazonka
import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified System.IO              as IO

data Config = Config
    { configRegion    :: !T.Text
    , configAccessKey :: !T.Text
    , configSecretKey :: !T.Text
    } deriving (Show)

-- | AWS region is not retrieved correctly from environment variables, and
-- neither from the AWS profile.
smartEnv :: Config -> IO Env
smartEnv conf = do
    logger' <- newLogger Info IO.stderr
    region' <- either fail pure $ Amazonka.fromText (configRegion conf)
    env <- newEnvNoAuth
    pure $ fromKeys
        (AccessKey $ T.encodeUtf8 $ configAccessKey conf)
        (SecretKey $ T.encodeUtf8 $ configSecretKey conf)
        env
        { logger = logger'
        , region = region'
        }

$(A.deriveJSON A.options ''Config)
