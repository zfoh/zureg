-- | Super-simple configuration module.
module Zureg.Config
    ( load
    , section
    ) where

import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T

type Config = Aeson.Object

load :: FilePath -> IO Config
load path = Aeson.eitherDecodeFileStrict' path >>= either fail return

section :: Aeson.FromJSON a => Config -> T.Text -> IO a
section obj key = case HMS.lookup key obj of
    Nothing  -> fail $ "Missing section " ++ T.unpack key ++ " in config"
    Just val -> case Aeson.fromJSON val of
        Aeson.Success x -> return x
        Aeson.Error str -> fail $
            "Error parsing section " ++ T.unpack key ++ ": " ++ str
