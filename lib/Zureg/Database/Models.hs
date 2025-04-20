{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Database.Models
    ( TShirtSize (..)
    , Region (..)
    , Occupation (..)
    , InsertRegistration (..)
    , RegistrationState (..)
    , parseRegistrationState
    , registrantCanJoinChat
    , Registration (..)
    , Project (..)
    ) where

import           Control.Exception                    (Exception)
import qualified Data.Aeson.TH.Extended               as A
import qualified Data.ByteString                      as B
import qualified Data.List                            as L
import qualified Data.Text                            as T
import qualified Data.Time                            as Time
import           Data.UUID                            (UUID)
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow   as Pg
import qualified Database.PostgreSQL.Simple.ToField   as Pg
import qualified Database.PostgreSQL.Simple.ToRow     as Pg
import           GHC.Generics                         (Generic)
import           Text.Read                            (readMaybe)

data TShirtSize = XS | S | M | L | XL | XXL
    deriving (Bounded, Enum, Eq, Read, Show)

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
    deriving (Bounded, Enum, Eq, Read, Show)

data Occupation
    = Student
    | Tech
    | Academia
    | Other
    deriving (Bounded, Enum, Eq, Read, Show)

data RegistrationState = Registered | Confirmed | Cancelled | Waitlisted | Spam
    deriving (Bounded, Enum, Eq, Read, Show)

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
    } deriving (Generic, Show)

data Registration = Registration
    { rID                    :: !UUID
    , rName                  :: !T.Text
    , rBadgeName             :: !(Maybe T.Text)
    , rEmail                 :: !T.Text
    , rAffiliation           :: !(Maybe T.Text)
    , rRegisteredAt          :: !Time.UTCTime
    , rTShirtSize            :: !(Maybe TShirtSize)
    , rRegion                :: !(Maybe Region)
    , rOccupation            :: !(Maybe Occupation)
    , rBeginnerTrackInterest :: !Bool
    , rState                 :: !RegistrationState
    , rScannedAt             :: !(Maybe Time.UTCTime)
    , rVip                   :: !Bool
    } deriving (Eq, Generic, Show)

instance Pg.ToField Region            where toField = Pg.toField . show
instance Pg.ToField TShirtSize        where toField = Pg.toField . show
instance Pg.ToField Occupation        where toField = Pg.toField . show
instance Pg.ToField RegistrationState where toField = Pg.toField . show

data ReadFieldError = ReadFieldError deriving (Show)

instance Exception ReadFieldError

readField :: Read a => Pg.Field -> Maybe B.ByteString -> Pg.Conversion a
readField field mbBS = do
    str <- Pg.fromField field mbBS
    case readMaybe str of
        Just x  -> pure x
        Nothing -> Pg.conversionError ReadFieldError

instance Pg.FromField Region            where fromField = readField
instance Pg.FromField TShirtSize        where fromField = readField
instance Pg.FromField Occupation        where fromField = readField
instance Pg.FromField RegistrationState where fromField = readField

instance Pg.ToRow InsertRegistration
instance Pg.FromRow Registration where

data Project = Project
    { pRegistrationID               :: !UUID
    , pName                         :: !T.Text
    , pLink                         :: !(Maybe T.Text)
    , pShortDescription             :: !(Maybe T.Text)
    , pContributorLevelBeginner     :: !Bool
    , pContributorLevelIntermediate :: !Bool
    , pContributorLevelAdvanced     :: !Bool
    } deriving (Eq, Generic, Show)

instance Pg.ToRow Project
instance Pg.FromRow Project

-- | This is a variation on MaybeT that will never short-circuit.  The reason
-- for this is that short-circuiting affects the number of columns expected
-- in a type by postgresql-simple.
newtype EagerMaybeT m a = EagerMaybeT {runEagerMaybe :: m (Maybe a)}

instance Functor m => Functor (EagerMaybeT m) where
    fmap f = EagerMaybeT . fmap (fmap f) . runEagerMaybe

instance Applicative m => Applicative (EagerMaybeT m) where
    pure      = EagerMaybeT . pure . Just
    fx <*> mx = EagerMaybeT $ (<*>) <$> runEagerMaybe fx <*> runEagerMaybe mx

instance Pg.FromRow (Maybe Project) where
    fromRow = runEagerMaybe $ Project
        <$> EagerMaybeT Pg.field <*> EagerMaybeT Pg.field
        <*> EagerMaybeT Pg.field <*> EagerMaybeT Pg.field
        <*> EagerMaybeT Pg.field <*> EagerMaybeT Pg.field
        <*> EagerMaybeT Pg.field

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''Occupation)
$(A.deriveJSON A.options ''RegistrationState)
