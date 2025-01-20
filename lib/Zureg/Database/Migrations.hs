{-# LANGUAGE OverloadedStrings #-}
module Zureg.Database.Migrations
    ( migrate
    ) where

import           Data.Char                  (isDigit)
import           Data.Foldable              (for_)
import           Data.List                  (sortOn)
import           Data.String                (fromString)
import           Data.Traversable           (for)
import qualified Database.PostgreSQL.Simple as Pg
import qualified System.Directory           as Directory
import           System.FilePath            ((</>))
import qualified System.IO                  as IO
import           Text.Read                  (readMaybe)
import           Zureg.Database.Internal

listMigrations :: Config -> IO [(Int, FilePath)]
listMigrations config = sortOn fst <$> do
    entries <- Directory.listDirectory $ configMigrations config
    for entries $ \entry -> do
        let path = configMigrations config </> entry
        case readMaybe (takeWhile isDigit entry) of
            Just v -> pure (v, path)
            _      -> fail $ "Could not parse version: " ++ path

migrate :: Handle -> IO ()
migrate h = do
    IO.hPutStrLn IO.stderr "Starting migrate..."
    _ <- withTransaction h $ \(Transaction conn) -> Pg.execute_ conn "\
        \CREATE TABLE IF NOT EXISTS migrations (\n\
        \    version INT NOT NULL PRIMARY KEY,\n\
        \    path TEXT NOT NULL\n\
        \)"

    migrations <- listMigrations $ hConfig h
    for_ migrations $ \(version, path) -> withTransaction h $
        \(Transaction conn) -> do
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
    IO.hPutStrLn IO.stderr "Migrate finished."
