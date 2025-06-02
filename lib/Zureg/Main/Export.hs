{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Zureg.Main.Export
    ( ExportRegistration (..)
    , ExportRegistrant (..)
    , ExportProject (..)
    , ExportContributorLevel (..)
    , fromModel

    , main
    ) where

import           Control.Monad              (when)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.TH.Extended     as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           Data.UUID                  (UUID)
import qualified Options.Applicative        as OA
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeExtension)
import qualified System.IO                  as IO
import qualified Zureg.Config               as Config
import qualified Zureg.Database             as Database
import           Zureg.Database.Models

data ExportRegistration = ExportRegistration
    { erID         :: !UUID
    , erConfirmed  :: !(Maybe Bool)  -- ^ work around mustache templating bullshit
    , erState      :: !RegistrationState
    , erRegistrant :: !ExportRegistrant
    , erProject    :: !(Maybe ExportProject)
    }

data ExportRegistrant = ExportRegistrant
    { erName      :: !T.Text
    , erBadgeName :: !(Maybe T.Text)
    , erEmail     :: !T.Text
    }

data ExportProject = ExportProject
    { epName             :: !T.Text
    , epLink             :: !(Maybe T.Text)
    , epShortDescription :: !(Maybe T.Text)
    , epContributorLevel :: !ExportContributorLevel
    }

data ExportContributorLevel = ExportContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''ExportContributorLevel)
$(A.deriveJSON A.options ''ExportRegistrant)
$(A.deriveJSON A.options ''ExportProject)
$(A.deriveJSON A.options ''ExportRegistration)

fromModel :: Registration -> Maybe Project -> ExportRegistration
fromModel registration mbProject = ExportRegistration
    { erID  = rID registration
    , erState = rState registration
    , erConfirmed = case rState registration of
        Confirmed -> Just True
        _ -> Nothing
    , erRegistrant = ExportRegistrant
        { erName = rName registration
        , erBadgeName = rBadgeName registration
        , erEmail = rEmail registration
        }
    , erProject = case mbProject of
        Nothing -> Nothing
        Just project -> Just ExportProject
            { epName = pName project
            , epLink = pLink project
            , epShortDescription = pShortDescription project
            , epContributorLevel = ExportContributorLevel
                { clBeginner = pContributorLevelBeginner project
                , clIntermediate = pContributorLevelIntermediate project
                , clAdvanced = pContributorLevelAdvanced project
                }
            }
    }

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

main :: IO ()
main = do
    opts     <- OA.execParser $
        OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc

    Config.Config {..} <- Config.load

    exists <- doesFileExist (oPath opts)
    when exists $ fail $ oPath opts ++ " already exists"

    let predicate = case oState opts of
            []        -> const True
            allowlist -> (`elem` allowlist) . erState

    encode <- case takeExtension (oPath opts) of
        ".json" -> return (A.encode :: [ExportRegistration] -> BL.ByteString)
        ext     -> do
            IO.hPutStrLn IO.stderr $ "Unknown extension: " ++ ext
            exitFailure

    Database.withHandle configDatabase $ \db -> do
        registrations <-
            fmap (map (uncurry fromModel)) $
            Database.withTransaction db $
            Database.selectRegistrationsWithProjects
        BL.writeFile (oPath opts) $ encode $ filter predicate registrations
