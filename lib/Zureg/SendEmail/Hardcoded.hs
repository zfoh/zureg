-- | A number of hardcoded emails.
{-# LANGUAGE OverloadedStrings #-}
module Zureg.SendEmail.Hardcoded
    ( sendRegisterSuccessEmail
    , sendWaitlistEmail
    , sendPopWaitlistEmail
    ) where

import qualified Data.Text       as T
import qualified Eventful        as E
import           Zureg.Model
import qualified Zureg.SendEmail as SendEmail

sendRegisterSuccessEmail
    :: SendEmail.Handle -> RegisterInfo -> E.UUID -> IO ()
sendRegisterSuccessEmail sendEmail info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    (hName hackathon <> " Registration Confirmation") $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "Your registration for " <> hName hackathon <> " was successful."
    , ""
    , "We look forward to seeing you there!"
    , ""
    , "You can view (and cancel) your registration here:"
    , ""
    , "    " <> hBaseUrl hackathon <> "/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    https://zfoh.ch/zurihac2019/#contact"
    , ""
    , "For various questions, or socializing with other attendees,"
    , "you can join our Slack organisation:"
    , ""
    , "    https://slack.zurihac.info/"
    , ""
    , "Warm regards"
    , "The " <> hName hackathon <> " Registration Bot"
    ]
  where
    hackathon = riHackathon info

sendWaitlistEmail
    :: SendEmail.Handle -> RegisterInfo -> E.UUID -> IO ()
sendWaitlistEmail sendEmail info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    "ZuriHac 2019: You're on the waitlist" $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "You have been added to the waitlist for ZuriHac 2019."
    , ""
    , "We will let you know when places become available."
    , ""
    , "You can view your status here:"
    , ""
    , "    https://zureg.zfoh.ch/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    https://zfoh.ch/zurihac2019/#contact"
    , ""
    , "Warm regards"
    , "The ZuriHac Registration Bot"
    ]

sendPopWaitlistEmail
    :: SendEmail.Handle -> RegisterInfo -> E.UUID -> IO ()
sendPopWaitlistEmail sendEmail info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    "ZuriHac 2019: You are now registered!" $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "Great news!  Some places for ZuriHac 2019 became available."
    , "You have been removed from the waiting list and are now"
    , "registered to attend ZuriHac."
    , ""
    , "You can view your registration here:"
    , ""
    , "    https://zureg.zfoh.ch/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    https://zfoh.ch/zurihac2019/#contact"
    , ""
    , "For various questions, or socializing with other attendees,"
    , "you can join our Slack organisation:"
    , ""
    , "    https://slack.zurihac.info/"
    , ""
    , "Warm regards"
    , "The ZuriHac Registration Bot"
    ]
