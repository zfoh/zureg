-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE TemplateHaskell #-}
module Zureg.Model
    ( TShirtModel (..)
    , TShirtSize (..)
    , RegisterInfo (..)
    , Event (..)

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
    | Cancel
    deriving (Show)

--------------------------------------------------------------------------------
-- State

data Registrant = Registrant
    { rUuid      :: E.UUID
    , rInfo      :: Maybe RegisterInfo
    , rCancelled :: Bool
    } deriving (Show)

registrantProjection :: E.UUID -> E.Projection Registrant Event
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing False
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rCancelled = True}
        Register i -> registrant {rInfo = Just i}
    }

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtModel)
$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''Registrant)
