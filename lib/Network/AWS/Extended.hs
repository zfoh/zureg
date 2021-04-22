module Network.AWS.Extended
    ( module Network.AWS
    , smartEnv
    ) where

import           Control.Lens       ((.~), (<&>))
import qualified Data.Text          as T
import           Network.AWS
import qualified Network.AWS.Data   as Aws
import           System.Environment (lookupEnv)

-- | AWS region is not retrieved correctly from environment variables, and
-- neither from the AWS profile.
smartEnv :: IO Env
smartEnv = do
    awsRegion <- regionFromEnv
    newEnv Discover <&> maybe id (envRegion .~) awsRegion
  where
    regionFromEnv = do
        maybeRegion <- lookupEnv "AWS_REGION"
        pure $ case maybeRegion of
            Just region -> case Aws.fromText $ T.pack region of
                Right r -> Just r
                Left _  -> Nothing
            Nothing -> Nothing
