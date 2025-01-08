{-# LANGUAGE OverloadedStrings #-}
module Zureg.Database.Migrations
    ( migrate
    ) where

import qualified Data.ByteString.Char8      as BS8
import           Data.Char                  (isDigit)
import           Data.Foldable              (for_)
import           Data.List                  (sortOn)
import           Data.String                (fromString)
import           Data.Traversable           (for)
import qualified Database.PostgreSQL.Simple as Pg
import qualified System.Directory           as Directory
import           System.Environment         (lookupEnv)
import           System.FilePath            ((</>))
import qualified System.IO                  as IO
import           Text.Read                  (readMaybe)

listMigrations :: IO [(Int, FilePath)]
listMigrations = sortOn fst <$> do
    entries <- Directory.listDirectory dir
    for entries $ \entry -> do
        let path = dir </> entry
        case readMaybe (takeWhile isDigit entry) of
            Just v -> pure (v, path)
            _      -> fail $ "Could not parse version: " ++ path
  where
    dir = "lib/Zureg/Database/Migrations"

migrate :: IO ()
migrate = do
    pgstring <- lookupEnv "ZUREG_DB" >>= maybe (fail "ZUREG_DB not set") pure
    conn <- Pg.connectPostgreSQL $ BS8.pack pgstring
    _ <- Pg.execute_ conn "\
        \CREATE TABLE IF NOT EXISTS migrations (\n\
        \    version INT NOT NULL PRIMARY KEY,\n\
        \    path TEXT NOT NULL\n\
        \)"

    migrations <- listMigrations
    for_ migrations $ \(version, path) -> Pg.withTransaction conn $ do
        rows <- Pg.query conn
            "SELECT version FROM migrations WHERE version = ?"
            (Pg.Only version) :: IO [Pg.Only Int]
        case rows of
            _ : _ ->
                IO.hPutStrLn IO.stderr $ "Skipping migration: " ++ path
            [] -> do
                IO.hPutStrLn IO.stderr $ "Running migration: " ++ path
                contents <- readFile path
                _ <- Pg.execute_ conn $ fromString contents
                _ <- Pg.execute conn
                    "INSERT INTO migrations (version, path) VALUES (?, ?)"
                    (version, path)
                pure ()

    Pg.close conn
