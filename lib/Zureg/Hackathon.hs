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
import qualified Zureg.Hackathon.ZuriHac2019 as ZuriHac2019
import qualified Zureg.Hackathon.ZuriHac2020 as ZuriHac2020

-- | Load the hackathon stored in the 'ZUREG_HACKATHON' environment variable.
withHackathonFromEnv
    :: (forall e r. ( Eq r
                    , Csv.ToNamedRecord r, Aeson.FromJSON r, Aeson.ToJSON r
                    , Aeson.FromJSON e, Aeson.ToJSON e
                    )
        => Hackathon e r -> IO a)
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
       forall e r. ( Eq r, Csv.ToNamedRecord r, Aeson.FromJSON r, Aeson.ToJSON r
                   , Aeson.FromJSON e, Aeson.ToJSON e
                   )
    => SomeHackathon (Hackathon e r)

hackathons :: [(String, IO SomeHackathon)]
hackathons =
    [ ("zurihac2019", SomeHackathon <$> ZuriHac2019.newHackathon)
    , ("zurihac2020", SomeHackathon <$> ZuriHac2020.newHackathon)
    ]
