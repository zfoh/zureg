-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Zureg.Model
    ( RegisterInfo (..)
    , WaitlistInfo (..)
    , PopWaitlistInfo (..)
    , ScanInfo (..)
    , UncancelInfo (..)
    , JoinChatInfo (..)
    , Event (..)

    , RegisterState (..)
    , Registrant (..)
    , registrantProjection

    , parseRegisterState
    , registrantRegisteredAt
    , registrantCanJoinChat
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Eventful               as E
import           Text.Read              (readMaybe)

--------------------------------------------------------------------------------
-- Events

data RegisterInfo = RegisterInfo
    { riName         :: !T.Text
    , riBadgeName    :: !(Maybe T.Text)
    , riEmail        :: !T.Text
    , riAffiliation  :: !(Maybe T.Text)
    , riRegisteredAt :: !Time.UTCTime
    } deriving (Eq, Show)

data WaitlistInfo = WaitlistInfo
    { wiWaitlistedAt :: !Time.UTCTime
    } deriving (Eq, Show)

data PopWaitlistInfo = PopWaitlistInfo
    { pwiPoppedAt :: !Time.UTCTime
    } deriving (Eq, Show)

data ScanInfo = ScanInfo
    { siScannedAt :: !Time.UTCTime
    } deriving (Eq, Show)

data UncancelInfo = UncancelInfo
    { uiUncanceledAt :: !Time.UTCTime
    } deriving (Eq, Show)

data JoinChatInfo = JoinChatInfo
    { gdiiJoinChatAt :: !Time.UTCTime
    } deriving (Eq, Show)

data Event a
    = Register RegisterInfo a
    | Waitlist WaitlistInfo
    | PopWaitlist PopWaitlistInfo
    | Scan ScanInfo
    | Confirm
    | Cancel
    | Uncancel UncancelInfo
    | JoinChat JoinChatInfo
    | MarkSpam
    | MarkVip
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- State

data RegisterState = Registered | Confirmed | Cancelled | Waitlisted | Spam
    deriving (Bounded, Enum, Eq, Read, Show)

data Registrant a = Registrant
    { rUuid           :: E.UUID
    , rInfo           :: Maybe RegisterInfo
    , rAdditionalInfo :: Maybe a
    , rState          :: Maybe RegisterState
    , rScanned        :: Bool
    , rVip            :: Bool
    } deriving (Eq, Show)

registrantProjection :: E.UUID -> E.Projection (Registrant a) (Event a)
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant
        { rUuid           = uuid
        , rInfo           = Nothing
        , rAdditionalInfo = Nothing
        , rState          = Nothing
        , rScanned        = False
        , rVip            = False
        }
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel | Just Spam /= rState registrant ->
            registrant {rState = Just Cancelled}
        Confirm -> case rState registrant of
            Just Registered -> registrant {rState = Just Confirmed}
            _               -> registrant
        Register i a -> registrant {rInfo = Just i, rAdditionalInfo = Just a, rState = Just Registered}
        Waitlist _ -> registrant {rState = Just Waitlisted}
        PopWaitlist _ | Just Waitlisted <- rState registrant ->
            registrant {rState = Just Registered}
        Scan _ -> registrant {rScanned = True}
        Uncancel _ | Just Cancelled <- rState registrant ->
            registrant {rState = Just Registered}
        MarkSpam -> registrant {rState = Just Spam}
        MarkVip -> registrant {rVip = True}
        _ -> registrant
    }

$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''WaitlistInfo)
$(A.deriveJSON A.options ''PopWaitlistInfo)
$(A.deriveJSON A.options ''ScanInfo)
$(A.deriveJSON A.options ''UncancelInfo)
$(A.deriveJSON A.options ''JoinChatInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''RegisterState)
$(A.deriveJSON A.options ''Registrant)

--------------------------------------------------------------------------------

parseRegisterState :: String -> Either String RegisterState
parseRegisterState str = case readMaybe str of
    Just rs -> return rs
    Nothing -> Left $
        "Can't parse register state, try one of: " ++
        L.intercalate ", " (map show [minBound :: RegisterState .. maxBound])

registrantRegisteredAt :: Registrant a -> Maybe Time.UTCTime
registrantRegisteredAt registrant = riRegisteredAt <$> rInfo registrant

registrantCanJoinChat :: Maybe RegisterState -> Bool
registrantCanJoinChat = \case
    Nothing         -> False
    Just Cancelled  -> False
    Just Registered -> True
    Just Confirmed  -> True
    Just Waitlisted -> False
    Just Spam       -> False
