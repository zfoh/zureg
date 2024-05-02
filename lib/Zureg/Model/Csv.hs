{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts,
    FlexibleInstances,
    RecordWildCards,
    OverloadedStrings
    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zureg.Model.Csv () where

import           Zureg.Model
import qualified Data.Time              as Time
import qualified Eventful               as E
import           Data.Csv               as CSV
import qualified Data.HashMap.Strict    as HM

instance ToNamedRecord a => ToNamedRecord (Registrant a) where
    toNamedRecord Registrant {..}
        =  HM.unions [ namedRecord [ "UUID" .= rUuid ]
                     , toNamedRecord rState
                     , toNamedRecord rInfo
                     , case rAdditionalInfo of
                        Just ai -> toNamedRecord ai
                        Nothing -> HM.empty
                     , namedRecord [ "Scanned" .= rScanned ]
                     , namedRecord [ "VIP" .= rVip ]
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
        = namedRecord [ "Name"               .= riName 
                      , "Name on Badge"      .= riBadgeName 
                      , "Email"              .= riEmail 
                      , "Affiliation"        .= riAffiliation 
                      , "Registered At"      .= riRegisteredAt
                      ]
         
           
instance ToField RegisterState where
    toField Registered = toField ("Registered" :: String)
    toField Confirmed  = toField ("Confirmed" :: String)
    toField Cancelled  = toField ("Cancelled" :: String)
    toField Waitlisted = toField ("Waitlisted" :: String)
    toField Spam       = toField ("Spam" :: String)

instance ToField Bool where
    toField True  = toField ("true" :: String)
    toField False = toField ("false" :: String)

instance ToField E.UUID where
    toField uuid' = toField (show uuid' :: String)

instance ToField Time.UTCTime where
    toField time' = toField (show time' :: String)
