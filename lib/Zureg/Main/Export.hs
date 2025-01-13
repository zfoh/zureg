{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Export
    ( main
    ) where

import           Control.Monad              (forM, when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Options.Applicative        as OA
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeExtension)
import qualified System.IO                  as IO
import qualified Zureg.Database             as Database
import           Zureg.Database.Models
import           Zureg.Hackathon            (Hackathon)

progressMapM :: (a -> IO b) -> [a] -> IO [b]
progressMapM f xs = forM (zip [1 :: Int ..] xs) $ \(n, x) -> do
    y <- f x
    when (n `mod` 10 == 0) $ IO.hPutStrLn IO.stderr $
        "Progress: " ++ show n ++ "/" ++ show len ++ "..."
    return y
  where
    len = length xs

data Options = Options
    { oState :: [RegistrationState]
    , oPath  :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.many (OA.option (OA.eitherReader parseRegistrationState) $
        OA.long "state" <>
        OA.help "filter registrants based on state")
    <*> OA.strArgument (
        OA.help    ".json export path" <>
        OA.metavar "PATH")

main :: Hackathon -> IO ()
main _ = do
    opts     <- OA.execParser $
        OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc

    exists <- doesFileExist (oPath opts)
    when exists $ fail $ oPath opts ++ " already exists"

    let predicate = case oState opts of
            []        -> const True
            allowlist -> (`elem` allowlist) . rState

    encode <- case takeExtension (oPath opts) of
        ".json" -> return (A.encode :: [Registration] -> BL.ByteString)
        ext     -> do
            IO.hPutStrLn IO.stderr $ "Unknown extension: " ++ ext
            exitFailure

    dbConfig <- Database.configFromEnv

    Database.withHandle dbConfig $ \db -> do
        uuids       <- Database.getRegistrantUuids db
        registrants <- progressMapM (Database.getRegistrant db) uuids
        BL.writeFile (oPath opts) $ encode $ filter predicate registrants
