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


main :: forall a. (A.FromJSON a, A.ToJSON a) => Hackathon a -> IO ()
main hackathon =
    Database.withHandle (Hackathon.databaseConfig hackathon) $ \_db ->
    Lambda.main IO.stdin IO.stdout (ErrorResponse . show) $ \Request ->
    pure $ MessageResponse "Hi, I'm the janitor!"
