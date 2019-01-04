{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts,
    FlexibleInstances,
    RecordWildCards,
    OverloadedStrings
    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zureg.Model.Csv
    ( itemHeader
    ) where

import           Zureg.Model
import qualified Data.Time              as Time
import qualified Eventful               as E
import           Data.Csv               as CSV
import qualified Data.HashMap.Strict    as HM

instance ToNamedRecord Registrant where
    toNamedRecord Registrant {..}
        =  HM.unions [ namedRecord [ "UUID" .= rUuid ]
                     , toNamedRecord rState
                     , toNamedRecord rInfo
                     ]

instance ToNamedRecord RegisterState where
    toNamedRecord registerState = namedRecord [ "State" .= registerState ]

instance ToNamedRecord RegisterState => ToNamedRecord (Maybe RegisterState) where
    toNamedRecord registerState = namedRecord [ "State" .= registerState ]

instance ToNamedRecord RegisterInfo => ToNamedRecord (Maybe RegisterInfo)  where
    toNamedRecord registerInfo = case registerInfo of
        Just registerInfo' -> toNamedRecord registerInfo'
        Nothing            -> namedRecord [ "Name" .= ("No Registration info present" :: String)]

instance ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}  
        = HM.unions [ namedRecord [ "Name"               .= riName 
                                  , "Name on Badge"      .= riBadgeName 
                                  , "Email"              .= riEmail 
                                  , "Affiliation"        .= riAffiliation 
                                  , "AskMeAbout"         .= riAskMeAbout 
                                  , "Beginner Track"     .= tiBeginner tiTrackInterest
                                  , "Intermediate Track" .= tiIntermediate tiTrackInterest
                                  , "Advanced Track"     .= tiAdvanced tiTrackInterest
                                  , "GhcDevOps Track"    .= tiGhcDevOps tiTrackInterest
                                  , "Mentor"             .= riMentor
                                  , "Registered At"      .= riRegisteredAt
                                  ]
                    , toNamedRecord riTShirt
                    , toNamedRecord riProject
                    ]

instance ToNamedRecord (TShirtCut, TShirtSize) where
    toNamedRecord (tShirtCut, tShirtSize) 
        = HM.unions [ namedRecord [ "T-Shirt Cut" .= tShirtCut
                                  , "T-Shirt Size" .= tShirtSize] 
                                  ]
                    
instance ToNamedRecord (TShirtCut, TShirtSize) => ToNamedRecord (Maybe (TShirtCut, TShirtSize)) where
    toNamedRecord tShirt = case tShirt of
        Just tShirt' -> toNamedRecord tShirt'
        Nothing      -> namedRecord [ "Name" .= ("No T-Shirt info present" :: String) ]

instance ToNamedRecord Project where
    toNamedRecord Project {..}
        = HM.unions [ namedRecord [ "Project Name"              .= pName
                                  , "Project Website"           .= pWebsite
                                  , "Project Short Description" .= pShortDescription
                                  ] 
                    , toNamedRecord pContributorLevel 
                    ]
            
instance ToNamedRecord ContributorLevel where
    toNamedRecord ContributorLevel {..}
        = namedRecord [ "CL Beginner"     .= clBeginner
                      , "CL Intermediate" .= clIntermediate
                      , "CL Advanced"     .= clAdvanced
                      ]
           
instance ToField RegisterState where
    toField Registered = toField ("Registered" :: String)
    toField Confirmed  = toField ("Confirmed" :: String)
    toField Cancelled  = toField ("Cancelled" :: String)

instance ToField Bool where
    toField True  = toField ("true" :: String)
    toField False = toField ("false" :: String)

instance ToField E.UUID where
    toField uuid' = toField (show uuid' :: String)

instance ToField Time.UTCTime where
    toField time' = toField (show time' :: String)

instance ToField TShirtCut where
    toField Female = toField ("female" :: String)
    toField Male   = toField ("male" :: String)

instance ToField TShirtSize where
    toField S   = toField ("S" :: String) 
    toField M   = toField ("M" :: String) 
    toField L   = toField ("L" :: String) 
    toField XL  = toField ("XL" :: String) 
    toField XXL = toField ("XXL" :: String) 

itemHeader :: Header
itemHeader = header
                [ "UUID"
                , "State"
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