{-# LANGUAGE TemplateHaskell #-}
module Zureg.Config
    ( Config (..)
    , load
    ) where

import qualified Data.Aeson                          as A
import qualified Data.Aeson.TH.Extended              as A
import qualified Data.Text                           as T
import           System.Directory                    (doesFileExist)
import qualified Zureg.AWS                           as AWS
import qualified Zureg.Captcha                       as Captcha
import qualified Zureg.Database                      as Database
import qualified Zureg.Hackathon                     as Hackathon
import qualified Zureg.Hackathon.ZuriHac2020.Discord as Discord
import qualified Zureg.Web                           as Web

data Config = Config
    { configHackathon     :: !Hackathon.Hackathon
    , configDatabase      :: !Database.Config
    , configDiscord       :: !Discord.Config
    , configCaptcha       :: !Captcha.Config
    , configAws           :: !AWS.Config
    , configWeb           :: !Web.Config
    , configScannerSecret :: !T.Text
    } deriving (Show)

$(A.deriveFromJSON A.options ''Config)

load :: IO Config
load = do
    local <- doesFileExist "zureg.json"
    let path = if local then "zureg.json" else "/etc/zureg.json"
    either fail pure =<< A.eitherDecodeFileStrict path
