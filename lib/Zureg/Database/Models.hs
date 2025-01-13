{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Database.Models
    ( TShirtSize (..)
    , Region (..)
    , Occupation (..)
    , ContributorLevel (..)
    , Project (..)
    , InsertRegistration (..)
    , RegistrationState (..)
    , parseRegistrationState
    , registrantCanJoinChat
    , Registration (..)
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Time              as Time
import           Data.UUID              (UUID)
import           Text.Read              (readMaybe)

data TShirtSize = XS | S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data Region
    = Switzerland
    | Europe
    | Africa
    | AmericaCentral
    | AmericaNorth
    | AmericaSouth
    | Asia
    | MiddleEast
    | Oceania
    deriving (Bounded, Enum, Eq, Show)

data Occupation
    = Student
    | Tech
    | Academia
    | Other
    deriving (Bounded, Enum, Eq, Show)

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Eq, Show)

data Project = Project
    { pName             :: !(Maybe T.Text)
    , pWebsite          :: !(Maybe T.Text)
    , pShortDescription :: !(Maybe T.Text)
    , pContributorLevel :: !ContributorLevel
    } deriving (Eq, Show)

data RegistrationState = Registered | Confirmed | Cancelled | Waitlisted | Spam
    deriving (Bounded, Enum, Eq, Read, Show)

-- TODO: move?
parseRegistrationState :: String -> Either String RegistrationState
parseRegistrationState str = case readMaybe str of
    Just rs -> return rs
    Nothing -> Left $
        "Can't parse register state, try one of: " ++
        L.intercalate ", " (map show [minBound :: RegistrationState .. maxBound])

-- TODO: move?
registrantCanJoinChat :: RegistrationState -> Bool
registrantCanJoinChat = \case
    Cancelled  -> False
    Registered -> True
    Confirmed  -> True
    Waitlisted -> False
    Spam       -> False

data InsertRegistration = InsertRegistration
    { irName                  :: !T.Text
    , irBadgeName             :: !(Maybe T.Text)
    , irEmail                 :: !T.Text
    , irAffiliation           :: !(Maybe T.Text)
    , irTShirtSize            :: !(Maybe TShirtSize)
    , irRegion                :: !(Maybe Region)
    , irOccupation            :: !(Maybe Occupation)
    , irBeginnerTrackInterest :: !Bool
    , irProject               :: !Project
    }

data Registration = Registration
    { rUuid                  :: !UUID
    , rName                  :: !T.Text
    , rBadgeName             :: !(Maybe T.Text)
    , rEmail                 :: !T.Text
    , rAffiliation           :: !(Maybe T.Text)
    , rRegisteredAt          :: !Time.UTCTime
    , rTShirtSize            :: !(Maybe TShirtSize)
    , rRegion                :: !(Maybe Region)
    , rOccupation            :: !(Maybe Occupation)
    , rBeginnerTrackInterest :: !Bool
    , rProject               :: !Project
    , rState                 :: !RegistrationState
    , rScanned               :: !Bool
    , rVip                   :: !Bool
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''Occupation)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegistrationState)
$(A.deriveJSON A.options ''Registration)
