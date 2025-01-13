{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
module Zureg.Hackathon
    ( Hackathon (..)
    , withHackathonFromEnv
    ) where

import qualified Data.Aeson                  as Aeson
import qualified Data.Csv                    as Csv
import           Data.List                   (intercalate)
import           Data.Maybe                  (fromMaybe)
import           System.Environment          (lookupEnv)
import           Zureg.Hackathon.Interface

-- | Load the hackathon stored in the 'ZUREG_HACKATHON' environment variable.
withHackathonFromEnv
    :: (Hackathon -> IO a) -> IO a
withHackathonFromEnv f = do
    mbHackathonName <- lookupEnv "ZUREG_HACKATHON"
    maybe (fail message) f (mbHackathonName >>= flip lookup hackathons)
  where
    message =
        "Environment variable ZUREG_HACKATHON should be set to one of: " ++
        intercalate ", " (map fst hackathons)

hackathons :: [(String, Hackathon)]
hackathons = []
