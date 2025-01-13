{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Database.Models
    ( TShirtSize (..)
    , Region (..)
    , Occupation (..)
    , ContributorLevel (..)
    , Project (..)
    , RegisterInfo (..)
    , csvHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import           Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Zureg.Model.Csv        ()

module Zureg.Database.Models
    ( TShirtSize (..)
    , Region (..)
    , Occupation (..)
    ) where

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
parseRegisterState :: String -> Either String RegisterState
parseRegisterState str = case readMaybe str of
    Just rs -> return rs
    Nothing -> Left $
        "Can't parse register state, try one of: " ++
        L.intercalate ", " (map show [minBound :: RegisterState .. maxBound])

-- TODO: move?
registrantCanJoinChat :: Maybe RegisterState -> Bool
registrantCanJoinChat = \case
    Nothing         -> False
    Just Cancelled  -> False
    Just Registered -> True
    Just Confirmed  -> True
    Just Waitlisted -> False
    Just Spam       -> False

data Registration = Registration
    { rUuid                  :: !UUID
    , rScanned               :: !Bool
    , rVip                   :: !Bool
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
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''Occupation)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToField TShirtSize where
    toField = toField . show

instance Csv.ToNamedRecord Project where
    toNamedRecord Project {..}
        = HM.unions [ namedRecord [ "Project Name"              .= pName
                                  , "Project Website"           .= pWebsite
                                  , "Project Short Description" .= pShortDescription
                                  ]
                    , toNamedRecord pContributorLevel
                    ]

instance Csv.ToNamedRecord ContributorLevel where
    toNamedRecord ContributorLevel {..}
        = namedRecord [ "CL Beginner"     .= clBeginner
                      , "CL Intermediate" .= clIntermediate
                      , "CL Advanced"     .= clAdvanced
                      ]

instance Csv.ToField Region where
    toField = toField . show

instance Csv.ToField Occupation where
    toField = toField . show

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = HM.unions [ namedRecord [ "Region"                  .= riRegion
                                  , "Occupation"              .= riOccupation
                                  , "T-Shirt Size"            .= riTShirtSize
                                  , "Beginner Track Interest" .= riBeginnerTrackInterest
                                  ]
                    , toNamedRecord riProject
                    ]

csvHeader :: Csv.Header
csvHeader = Csv.header
    [ "UUID"
    , "State"
    , "Scanned"
    , "VIP"
    , "Name"
    , "Email"
    , "Region"
    , "Occupation"
    , "Project Name"
    , "Project Website"
    , "Project Short Description"
    , "CL Beginner"
    , "CL Intermediate"
    , "CL Advanced"
    , "Registered At"
    , "T-Shirt Size"
    , "Beginner Track Interest"
    ]
