{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Email
    ( main
    ) where

import           Control.Monad      (forM_, unless)
import qualified Data.Aeson         as A
import qualified Data.HashSet       as HS
import           Data.Maybe         (listToMaybe, maybe)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import qualified System.IO          as IO
import qualified Text.Mustache      as Mustache
import           Zureg.Hackathon    (Hackathon)
import qualified Zureg.Hackathon    as Hackathon
import           Zureg.Model
import qualified Zureg.SendEmail    as SendEmail

withStateFile
    :: FilePath -> (HS.HashSet T.Text -> (T.Text -> IO ()) -> IO a) -> IO a
withStateFile path f = do
    exists <- doesFileExist path
    done   <- fmap HS.fromList $
        if exists then T.lines <$> T.readFile path else return []
    IO.withFile path IO.AppendMode $ \h ->
        f done (\item -> T.hPutStrLn h item >> IO.hFlush h)

confirm :: IO ()
confirm = do
    putStr "Please confirm by typing \"yes\": "
    IO.hFlush IO.stdout
    line <- getLine
    unless (line == "yes") $ fail "aborted"

main :: forall e a. (A.FromJSON a, A.ToJSON a) => Hackathon e a -> IO ()
main Hackathon.Hackathon {..} = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath, templatePath, statefile, subject] -> do
            templateOrError <- Mustache.localAutomaticCompile templatePath
            template <- either (fail . show) return templateOrError

            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant a]

            let prepare :: Registrant a -> IO T.Text
                prepare registrant = do
                    let (errs, t) = Mustache.checkedSubstitute
                            template (A.toJSON registrant)
                    forM_ errs $ IO.hPutStrLn IO.stderr . show
                    return t

            r0 <- maybe (fail "No mails to send") return (listToMaybe registrants)
            t0 <- prepare r0
            putStrLn $ "Subject: " ++ subject ++ "\n"
            T.putStr t0
            confirm

            withStateFile statefile $ \done append ->
                SendEmail.withHandle sendEmailConfig $ \sendEmail ->
                forM_ registrants $ \registrant -> case rInfo registrant of
                Just ri | not (riEmail ri `HS.member` done) -> do
                    putStrLn $ "Mailing " ++ T.unpack (riEmail ri) ++ "..."
                    t <- prepare registrant
                    SendEmail.sendEmail sendEmail (riEmail ri) (T.pack subject) t
                    append (riEmail ri)

                _ -> return ()

        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json template.txt " ++
                    "statefile subject"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                , ""
                , "template.txt is a mustache template."
                , ""
                , "statefile is used to store which emails have been sent"
                , "already so we can resume if the process is killed.  If it"
                , "does not exist it is created."
                , ""
                , "subject is the subject of the email."
                ]
            exitFailure
