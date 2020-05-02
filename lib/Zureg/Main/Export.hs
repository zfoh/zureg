{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Zureg.Database             as Database
import           Zureg.Hackathon            (Hackathon)
import qualified Zureg.Hackathon            as Hackathon
import           Zureg.Model
import           Zureg.Model.Csv            ()

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

main
    :: forall e a. ( A.FromJSON e
                   , CSV.ToNamedRecord a, A.FromJSON a, A.ToJSON a
                   )
     => Hackathon e a -> IO ()
main Hackathon.Hackathon {..} = do
    opts     <- OA.execParser $
        OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc

    exists <- doesFileExist (oPath opts)
    when exists $ fail $ oPath opts ++ " already exists"

    let predicate = case oState opts of
            Nothing -> const True
            Just s  -> (== Just s) . rState

    encode <- case takeExtension (oPath opts) of
        ".json" -> return (A.encode :: [Registrant a] -> BL.ByteString)
        ".csv"  -> return $ CSV.encodeByName csvHeader
        ext     -> do
            IO.hPutStrLn IO.stderr $ "Unknown extension: " ++ ext
            exitFailure

    Database.withHandle databaseConfig $ \db -> do
        uuids       <- Database.getRegistrantUuids db
        registrants <- progressMapM
            (Database.getRegistrant db customEventHandler) uuids
        BL.writeFile (oPath opts) $ encode $ filter predicate registrants
