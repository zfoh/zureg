-- | Discord integration for ZuriHac 2020.
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module Zureg.Hackathon.ZuriHac2020.Discord
    ( Config (..)
    , configFromEnv
    , aboutMe
    , getWelcomeChannelId
    , generateTempInviteUrl
    ) where

import qualified Data.Aeson              as Aeson
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Void               (Void)
import           GHC.Generics            (Generic)
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import           System.Environment      (getEnv)

data Config = Config
    { accessToken :: !T.Text
    , guildId     :: !T.Text
    } deriving (Show)

configFromEnv :: IO Config
configFromEnv = Config
    <$> (T.pack <$> getEnv "ZUREG_DISCORD_ACCESS_TOKEN")
    <*> (T.pack <$> getEnv "ZUREG_DISCORD_GUILD_ID")

apiEndpoint :: T.Text
apiEndpoint = "https://discordapp.com/api/v6"

data Request a where
    Get  :: Request Void
    Post :: a -> Request a

request
    :: (Aeson.ToJSON a, Aeson.FromJSON b)
    => Config -> T.Text -> Request a -> IO b
request conf path req = do
    manager <- Http.newTlsManager
    req0 <- Http.parseRequest $ T.unpack $ apiEndpoint <> path
    let req1 = req0
            { Http.method = case req of Get -> "GET"; Post _ -> "POST"
            , Http.requestHeaders =
                ("Accept", "application/json") :
                ("Authorization", T.encodeUtf8 $ "Bot " <> accessToken conf) :
                [("Content-Type", "application/json") | Post _ <- [req]] ++
                Http.requestHeaders req0
            , Http.requestBody = case req of
                Get    -> Http.requestBody req0
                Post b -> Http.RequestBodyLBS (Aeson.encode b)
            }
    response <- Http.httpLbs req1 manager
    either (fail . ("Error decoding request: " ++)) pure $
        Aeson.eitherDecode' (Http.responseBody response)

aboutMe :: Config -> IO Aeson.Value
aboutMe creds = request creds "/oauth2/applications/@me" Get

data Guild = Guild
    { system_channel_id :: !T.Text
    } deriving (Generic, Show)

instance Aeson.FromJSON Guild

getWelcomeChannelId :: Config -> IO T.Text
getWelcomeChannelId conf =
    fmap system_channel_id $
    request conf ("/guilds/" <> guildId conf) Get

data Invite = Invite
    { code :: !T.Text
    } deriving (Generic, Show)

instance Aeson.FromJSON Invite

inviteToUrl :: Invite -> T.Text
inviteToUrl invite = "https://discord.gg/" <> code invite

generateTempInviteUrl :: Config -> T.Text -> IO T.Text
generateTempInviteUrl conf channelId =
    fmap inviteToUrl $
    request conf ("/channels/" <> channelId <> "/invites") $ Post $ Aeson.object
    [ "max_uses" Aeson..= (1 :: Int)
    , "max_age"  Aeson..= (10 * 60 :: Int)
    ]
