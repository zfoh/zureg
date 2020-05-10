-- | A number of hardcoded emails.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.SendEmail.Hardcoded
    ( sendRegisterSuccessEmail
    , sendWaitlistEmail
    , sendPopWaitlistEmail
    ) where

import qualified Data.Text       as T
import qualified Eventful        as E
import           Zureg.Hackathon
import           Zureg.Model
import qualified Zureg.SendEmail as SendEmail

sendRegisterSuccessEmail
    :: SendEmail.Handle -> Hackathon a -> RegisterInfo -> E.UUID -> IO ()
sendRegisterSuccessEmail sendEmail Hackathon {..} info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    (name <> " Registration Confirmation") $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "Your registration for " <> name <> " was successful."
    , ""
    , "We look forward to seeing you there!"
    , ""
    , "You can view your registration and join our chat here:"
    , ""
    , "    " <> baseUrl <> "/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    " <> contactUrl
    , ""
    , ""
    , "Warm regards"
    , "The " <> name <> " Registration Bot"
    ]

sendWaitlistEmail
    :: SendEmail.Handle -> Hackathon a -> RegisterInfo -> E.UUID -> IO ()
sendWaitlistEmail sendEmail Hackathon {..} info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    (name <> ": You're on the waitlist") $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "You have been added to the waitlist for " <> name <> "."
    , ""
    , "We will let you know when places become available."
    , ""
    , "You can view your status here:"
    , ""
    , "    " <> baseUrl <> "/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    " <> contactUrl
    , ""
    , "Warm regards"
    , "The " <> name <> " Registration Bot"
    ]

sendPopWaitlistEmail
    :: SendEmail.Handle -> Hackathon a -> RegisterInfo -> E.UUID -> IO ()
sendPopWaitlistEmail sendEmail Hackathon {..} info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    (name <> ": You are now registered!") $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "Great news!  Some places for " <> name <> " became available."
    , "You have been removed from the waiting list and are now"
    , "registered to attend " <> name <> "."
    , ""
    , "You can view your registration and join our chat here:"
    , ""
    , "    " <> baseUrl <> "/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    " <> contactUrl
    , ""
    , "Warm regards"
    , "The " <> name <> " Registration Bot"
    ]
