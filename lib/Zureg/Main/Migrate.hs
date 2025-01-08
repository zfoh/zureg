{-# LANGUAGE OverloadedStrings #-}
module Zureg.Main.Migrate (main) where

import           Zureg.Database.Migrations (migrate)

main :: IO ()
main = migrate
