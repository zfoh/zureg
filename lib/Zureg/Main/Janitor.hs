{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Janitor
    ( popWaitlist
    , main
    ) where

import           Data.Foldable             (for_)
import qualified Data.Text                 as T
import qualified System.IO                 as IO
import qualified Zureg.Config              as Config
import qualified Zureg.Database            as Database
import           Zureg.Database.Models
import qualified Zureg.Hackathon           as Hackathon
import           Zureg.Hackathon           (Hackathon)
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded

popWaitlist
    :: Database.Handle
    -> SendEmail.Handle
    -> Hackathon
    -> IO ()
popWaitlist db mailer hackathon@Hackathon.Hackathon {..} = do
    popped <- Database.withTransaction db $ \tx -> do
        attending <- Database.selectAttending tx
        let open = max 0 $ capacity - attending
        pop <- take open <$> Database.selectWaitlist tx
        for_ pop $ \registrant ->
            Database.setRegistrationState tx (rUuid registrant) Registered
        pure pop
    -- Send emails outside of transaction to make it faster
    for_ popped $ \registrant -> do
        IO.hPutStrLn IO.stderr $
            "Mailing " ++ T.unpack (rEmail registrant) ++ "..."
        sendPopWaitlistEmail mailer hackathon registrant

main :: IO ()
main = do
    Config.Config {..} <- Config.load
    Database.withHandle configDatabase $ \db ->
        SendEmail.withHandle configAws $ \mailer ->
            popWaitlist db mailer configHackathon
