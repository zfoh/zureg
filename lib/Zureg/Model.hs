-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE TemplateHaskell #-}
module Zureg.Model
    ( TShirtModel (..)
    , TShirtSize (..)
    , RegisterInfo (..)
    , Event (..)

    , RegisterState (..)
    , Registrant (..)
    , registrantProjection
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Eventful               as E

--------------------------------------------------------------------------------
-- Events

data TShirtModel = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data RegisterInfo = RegisterInfo
    { riName                :: !T.Text
    , riBadgeLine1          :: !(Maybe T.Text)
    , riBadgeLine2          :: !(Maybe T.Text)
    , riEmail               :: !T.Text
    , riEmailConfirm        :: !T.Text
    , riPublicParticipation :: !Bool
    , riTShirtModel         :: !(Maybe TShirtModel)
    , riTShirtSize          :: !(Maybe TShirtSize)
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

registrantProjection :: E.UUID -> E.Projection Registrant Event
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing Nothing
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rState = Just Cancelled}
        Confirm    -> registrant {rState = Just Confirmed}
        Register i -> registrant {rInfo = Just i, rState = Just Registered}
    }

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtModel)
$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''RegisterState)
$(A.deriveJSON A.options ''Registrant)
