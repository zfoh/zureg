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
import qualified Zureg.Hackathon.ZuriHac2024 as ZuriHac2024

-- | Load the hackathon stored in the 'ZUREG_HACKATHON' environment variable.
withHackathonFromEnv
    :: (forall r. (Eq r, Csv.ToNamedRecord r, Aeson.FromJSON r, Aeson.ToJSON r)
        => Hackathon r -> IO a)
    -> IO a
withHackathonFromEnv f = do
    mbHackathonName <- lookupEnv "ZUREG_HACKATHON"
    sh <- fromMaybe (fail message) (mbHackathonName >>= flip lookup hackathons)
    case sh of SomeHackathon h -> f h
  where
    message =
        "Environment variable ZUREG_HACKATHON should be set to one of: " ++
        intercalate ", " (map fst hackathons)

data SomeHackathon =
       forall r. (Eq r, Csv.ToNamedRecord r, Aeson.FromJSON r, Aeson.ToJSON r)
    => SomeHackathon (Hackathon r)

hackathons :: [(String, IO SomeHackathon)]
hackathons =
    [ ("zurihac2024", SomeHackathon <$> ZuriHac2024.newHackathon)
    ]
