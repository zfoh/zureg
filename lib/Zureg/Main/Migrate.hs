module Zureg.Main.Migrate (main) where

import qualified Zureg.Config   as Config
import qualified Zureg.Database as Database

main :: IO ()
main = do
    conf <- Config.load
    Database.withHandle (Config.configDatabase conf) Database.migrate
