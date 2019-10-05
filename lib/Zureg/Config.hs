-- | Super-simple configuration module.
module Zureg.Config
    ( Config (..)
    , load
    , section
    ) where

import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T

newtype Config = Config Aeson.Object

load :: FilePath -> IO Config
load path = Aeson.eitherDecodeFileStrict' path >>= either fail (return . Config)

section :: Aeson.FromJSON a => T.Text -> Config -> IO a
section key (Config obj) = case HMS.lookup key obj of
    Nothing  -> fail $ "Missing section " ++ T.unpack key ++ " in config"
    Just val -> case Aeson.fromJSON val of
        Aeson.Success x -> return x
        Aeson.Error str -> fail $
            "Error parsing section " ++ T.unpack key ++ ": " ++ str