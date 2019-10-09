{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.ZuriHac2019.Model
    ( TShirtCut (..)
    , TShirtSize (..)
    , TrackInterest (..)
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

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TrackInterest = TrackInterest
    { tiBeginner     :: !Bool
    , tiIntermediate :: !Bool
    , tiAdvanced     :: !Bool
    , tiGhcDevOps    :: !Bool
    } deriving (Eq, Show)

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

data RegisterInfo = RegisterInfo
    { tiTrackInterest :: !TrackInterest
    , riTShirt        :: !(Maybe (TShirtCut, TShirtSize))
    , riMentor        :: !Bool
    , riProject       :: !Project
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtCut)
$(A.deriveJSON A.options ''TrackInterest)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToNamedRecord (Maybe (TShirtCut, TShirtSize)) where
    toNamedRecord mbTshirt =
        namedRecord [ "T-Shirt Cut" .= fmap fst mbTshirt
                    , "T-Shirt Size" .= fmap snd mbTshirt
                    ]

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

instance Csv.ToField TShirtCut where
    toField Female = toField ("female" :: String)
    toField Male   = toField ("male" :: String)

instance Csv.ToField TShirtSize where
    toField S   = toField ("S" :: String)
    toField M   = toField ("M" :: String)
    toField L   = toField ("L" :: String)
    toField XL  = toField ("XL" :: String)
    toField XXL = toField ("XXL" :: String)

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = HM.unions [ namedRecord [ "Beginner Track"     .= tiBeginner tiTrackInterest
                                  , "Intermediate Track" .= tiIntermediate tiTrackInterest
                                  , "Advanced Track"     .= tiAdvanced tiTrackInterest
                                  , "GhcDevOps Track"    .= tiGhcDevOps tiTrackInterest
                                  , "Mentor"             .= riMentor
                                  ]
                    , toNamedRecord riTShirt
                    , toNamedRecord riProject
                    ]

csvHeader :: Csv.Header
csvHeader = Csv.header
    [ "UUID"
    , "State"
    , "Scanned"
    , "Name"
    , "Name on Badge"
    , "Email"
    , "Affiliation"
    , "AskMeAbout"
    , "Beginner Track"
    , "Intermediate Track"
    , "Advanced Track"
    , "GhcDevOps Track"
    , "Mentor"
    , "T-Shirt Cut"
    , "T-Shirt Size"
    , "Mentor"
    , "Project Name"
    , "Project Website"
    , "Project Short Description"
    , "CL Beginner"
    , "CL Intermediate"
    , "CL Advanced"
    , "Registered At"
    ]
