{-# LANGUAGE OverloadedStrings #-}
module Zureg.Main.Export
    ( main
    ) where

import           Control.Monad              (when, forM)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import qualified Data.Csv                   as CSV
import qualified System.IO                  as IO
import qualified Zureg.Config               as Config
import qualified Zureg.Database             as Database
import qualified Zureg.Model                as Model

progressMapM :: (a -> IO b) -> [a] -> IO [b]
progressMapM f xs = forM (zip [1 :: Int ..] xs) $ \(n, x) -> do
    y <- f x
    when (n `mod` 10 == 0) $ IO.hPutStrLn IO.stderr $
        "Progress: " ++ show n ++ "/" ++ show len ++ "..."
    return y
  where
    len = length xs

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    config   <- Config.load "zureg.json"
    dbConfig <- Config.section config "database"

    case args of
        [path, format] -> do
            exists <- doesFileExist path
            when exists $ fail $ path ++ " already exists"
            case format of
                "json" -> Database.withHandle dbConfig $ \db -> do
                        uuids       <- Database.getRegistrantUuids db
                        registrants <- progressMapM (Database.getRegistrant db) uuids
                        BL.writeFile path $ A.encode registrants
                "csv" -> Database.withHandle dbConfig $ \db -> do
                        uuids       <- Database.getRegistrantUuids db
                        registrants <- progressMapM (Database.getRegistrant db) uuids
                        BL.writeFile path $ CSV.encodeByName Model.itemHeader registrants
                _   -> do 
                    IO.hPutStr IO.stderr $ unlines
                        [ "Unkown format: " ++ format ]
                    exitFailure   

        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " <filename> <format>"
                , ""
                , "Export registrants from the database into a JSON file"
                ]
            exitFailure
