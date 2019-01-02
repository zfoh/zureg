-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts,
    FlexibleInstances
    #-}

module Zureg.Model
    ( TShirtCut (..)
    , TShirtSize (..)
    , TrackInterest (..)
    , ContributorLevel (..)
    , Project (..)
    , RegisterInfo (..)
    , Event (..)

    , RegisterState (..)
    , Registrant (..)
    , registrantProjection
    , itemHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Eventful               as E
import           Data.Csv               as CSV
import qualified Data.HashMap.Strict    as HM
import qualified Data.ByteString.Char8  as C

--------------------------------------------------------------------------------
-- Events

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TrackInterest = TrackInterest
    { tiBeginner     :: !Bool
    , tiIntermediate :: !Bool
    , tiAdvanced     :: !Bool
    , tiGhcDevOps    :: !Bool
    } deriving (Show)

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Show)

data Project = Project
    { pName             :: !(Maybe T.Text)
    , pWebsite          :: !(Maybe T.Text)
    , pShortDescription :: !(Maybe T.Text)
    , pContributorLevel :: !ContributorLevel
    } deriving (Show)

data RegisterInfo = RegisterInfo
    { riName          :: !T.Text
    , riBadgeName     :: !(Maybe T.Text)
    , riEmail         :: !T.Text
    , riAffiliation   :: !(Maybe T.Text)
    , riAskMeAbout    :: !(Maybe T.Text)
    , tiTrackInterest :: !TrackInterest
    , riTShirt        :: !(Maybe (TShirtCut, TShirtSize))
    , riMentor        :: !Bool
    , riProject       :: !Project
    -- | This field is optional because we only added after registration had
    -- been opened.
    , riRegisteredAt  :: !(Maybe Time.UTCTime)
    } deriving (Show)

data Event
    = Register RegisterInfo
    | Confirm
    | Cancel
    deriving (Show)

--------------------------------------------------------------------------------
-- State

data RegisterState = Registered | Confirmed | Cancelled
    deriving (Eq, Show)

data Registrant = Registrant
    { rUuid  :: E.UUID
    , rInfo  :: Maybe RegisterInfo
    , rState :: Maybe RegisterState
    } deriving (Show)

instance ToNamedRecord Registrant where
    toNamedRecord (Registrant rUuid' rInfo' rState')
        =  HM.unions [ namedRecord [ C.pack "UUID" .= rUuid' ]
                     , toNamedRecord rState'
                     , toNamedRecord rInfo'
                     ]

instance ToNamedRecord RegisterState where
    toNamedRecord registerState = namedRecord [ C.pack "State" .= registerState ]

instance ToNamedRecord RegisterState => ToNamedRecord (Maybe RegisterState) where
    toNamedRecord registerState = namedRecord [ C.pack "State" .= registerState ]

instance ToNamedRecord RegisterInfo => ToNamedRecord (Maybe RegisterInfo)  where
    toNamedRecord registerInfo = case registerInfo of
        Just registerInfo' -> toNamedRecord registerInfo'
        Nothing            -> namedRecord [ C.pack "Name" .= "No Registration info present"]

instance ToNamedRecord RegisterInfo where
    toNamedRecord (RegisterInfo  riName' riBadgeName' riEmail' riAffiliation' riAskMeAbout' 
                    tiTrackInterest' riTShirt' riMentor' riProject' riRegisteredAt')  
        = HM.unions [ namedRecord [ C.pack "Name"               .= riName' 
                                  , C.pack "Name on Badge"      .= riBadgeName' 
                                  , C.pack "Email"              .= riEmail' 
                                  , C.pack "Affiliation"        .= riAffiliation' 
                                  , C.pack "AskMeAbout"         .= riAskMeAbout' 
                                  , C.pack "Beginner Track"     .= tiBeginner tiTrackInterest'
                                  , C.pack "Intermediate Track" .= tiIntermediate tiTrackInterest'
                                  , C.pack "Advanced Track"     .= tiAdvanced tiTrackInterest'
                                  , C.pack "GhcDevOps Track"    .= tiGhcDevOps tiTrackInterest'
                                  , C.pack "Mentor"             .= riMentor'
                                  , C.pack "Registered At"      .= riRegisteredAt'
                                  ]
                    , toNamedRecord riTShirt'
                    , toNamedRecord riProject'
                    ]

instance ToNamedRecord (TShirtCut, TShirtSize) where
    toNamedRecord (tShirtCut, tShirtSize) 
        = HM.unions [ namedRecord [ C.pack "T-Shirt Cut" .= tShirtCut
                                  , C.pack "T-Shirt Size" .= tShirtSize] 
                                  ]
                    
instance ToNamedRecord (TShirtCut, TShirtSize) => ToNamedRecord (Maybe (TShirtCut, TShirtSize)) where
    toNamedRecord tShirt = case tShirt of
        Just tShirt' -> toNamedRecord tShirt'
        Nothing      -> namedRecord [ C.pack "Name" .= "No T-Shirt info present"]

instance ToNamedRecord Project where
    toNamedRecord (Project pName' pWebsite' pShortDescription' pContributorLevel')
        = HM.unions [ namedRecord [ C.pack "Project Name"              .= pName'
                                  , C.pack "Project Website"           .= pWebsite'
                                  , C.pack "Project Short Description" .= pShortDescription'
                                  ] 
                    , toNamedRecord pContributorLevel' 
                    ]
            
instance ToNamedRecord ContributorLevel where
    toNamedRecord (ContributorLevel clBeginner' clIntermediate' clAdvanced') 
        = namedRecord [ C.pack "CL Beginner"     .= clBeginner'
                      , C.pack "CL Intermediate" .= clIntermediate'
                      , C.pack "CL Advanced"     .= clAdvanced'
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
itemHeader = header $ map C.pack 
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

registrantProjection :: E.UUID -> E.Projection Registrant Event
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing Nothing
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rState = Just Cancelled}
        Confirm    -> registrant {rState = Just Confirmed}
        Register i -> registrant {rInfo = Just i, rState = Just Registered}
    }

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtCut)
$(A.deriveJSON A.options ''TrackInterest)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''RegisterState)
$(A.deriveJSON A.options ''Registrant)
