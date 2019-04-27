{-# LANGUAGE OverloadedStrings #-}
module Zureg.Main.Export
    ( main
    ) where

import           Control.Monad              (forM, when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv                   as CSV
import qualified Options.Applicative        as OA
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeExtension)
import qualified System.IO                  as IO
import qualified Zureg.Config               as Config
import qualified Zureg.Database             as Database
import           Zureg.Model
import qualified Zureg.Model.Csv            as MCSV

progressMapM :: (a -> IO b) -> [a] -> IO [b]
progressMapM f xs = forM (zip [1 :: Int ..] xs) $ \(n, x) -> do
    y <- f x
    when (n `mod` 10 == 0) $ IO.hPutStrLn IO.stderr $
        "Progress: " ++ show n ++ "/" ++ show len ++ "..."
    return y
  where
    len = length xs

data Options = Options
    { oState :: Maybe RegisterState
    , oPath  :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.option (OA.eitherReader parseRegisterState) $
        OA.long "state" <>
        OA.help "filter registrants based on state")
    <*> OA.strArgument (
        OA.help    ".csv or .json export path" <>
        OA.metavar "PATH")

main :: IO ()
main = do
    opts     <- OA.execParser $
        OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
    config   <- Config.load "zureg.json"
    dbConfig <- Config.section config "database"

    exists <- doesFileExist (oPath opts)
    when exists $ fail $ oPath opts ++ " already exists"

    let predicate = case oState opts of
            Nothing -> const True
            Just s  -> (== Just s) . rState

    encode <- case takeExtension (oPath opts) of
        ".json" -> return A.encode
        ".csv"  -> return $ CSV.encodeByName MCSV.itemHeader
        ext     -> do
            IO.hPutStrLn IO.stderr $ "Unkown extension: " ++ ext
            exitFailure

    Database.withHandle dbConfig $ \db -> do
        uuids       <- Database.getRegistrantUuids db
        registrants <- progressMapM (Database.getRegistrant db) uuids
        BL.writeFile (oPath opts) $ encode $ filter predicate registrants
