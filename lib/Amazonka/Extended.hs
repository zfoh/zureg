module Amazonka.Extended
    ( module Amazonka
    , smartEnv
    ) where

import           Amazonka
import qualified Amazonka.Data      as Amazonka
import qualified Data.Text          as T
import           System.Environment (lookupEnv)
import qualified System.IO          as IO

-- | AWS region is not retrieved correctly from environment variables, and
-- neither from the AWS profile.
smartEnv :: IO Env
smartEnv = do
    logger' <- newLogger Debug IO.stderr
    maybeRegion <- lookupEnv "AWS_REGION"
    region' <- case maybeRegion of
        Just str | Right r <- Amazonka.fromText $ T.pack str -> pure r
        _ -> fail "AWS_REGION needs to be set"
    env <- newEnv discover
    pure env
        { logger = logger'
        , region = region'
        }
