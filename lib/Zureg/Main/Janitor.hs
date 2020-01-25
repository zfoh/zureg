{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Janitor
    ( main
    ) where

import qualified Data.Aeson      as A
import qualified System.IO       as IO
import qualified Zureg.Database  as Database
import           Zureg.Hackathon (Hackathon)
import qualified Zureg.Hackathon as Hackathon
import qualified Zureg.Lambda    as Lambda
import           Zureg.Model
import Data.Maybe



--------------------------------------------------------------------------------
-- The request and response types for the janitor lambda are mostly ignored,
-- it's just a thing that runs from time to time.

data Request = Request

instance A.FromJSON Request where
    parseJSON _ = pure Request

data Response
    = MessageResponse String
    | ErrorResponse   String

instance A.ToJSON Response where
    toJSON (MessageResponse msg) = A.object ["message" A..= msg]
    toJSON (ErrorResponse   err) = A.object ["error"   A..= err]

countByState ::(RegisterState -> Bool) -> [Registrant a] -> Int
countByState f registrants = length $ filter f $ mapMaybe rState registrants

isWaiting :: RegisterState -> Bool
isWaiting Waitlisted = True
isWaiting _ = False

isConfirmed :: RegisterState -> Bool
isConfirmed Confirmed = True
isConfirmed _ = False

isAttending :: RegisterState -> Bool
isAttending Confirmed = True
isAttending Registered = True
isAttending _ = False

main :: forall a. (A.FromJSON a, A.ToJSON a) => Hackathon a -> IO ()
main hackathon =
    Database.withHandle (Hackathon.databaseConfig hackathon) $ \db ->
    Lambda.main IO.stdin IO.stdout (ErrorResponse . show) $ \Request -> do
    uuids       <- Database.getRegistrantUuids db
    registrants <- mapM (Database.getRegistrant db) uuids :: IO [Registrant a]

    let capacity = Hackathon.capacity hackathon
    let attending = countByState isAttending registrants

    let summary = Database.RegistrantsSummary
            { Database.rsTotal = length registrants
            , Database.rsWaiting  = countByState isWaiting registrants
            , Database.rsConfirmed = countByState isConfirmed registrants
            , Database.rsAttending = attending
            , Database.rsAvailable = capacity - attending
            }

    Database.putRegistrantsSummary db summary
    pure $ MessageResponse $ "Computed summary: " ++ renderSummary summary

renderSummary :: Database.RegistrantsSummary -> String
renderSummary rs = show (Database.rsTotal rs) ++ " total, " ++
  show (Database.rsWaiting rs) ++ " waiting, " ++
  show (Database.rsAttending rs) ++ " attending, " ++
  show (Database.rsConfirmed rs) ++ " confirmed, " ++
  show (Database.rsAvailable rs) ++ " available"
