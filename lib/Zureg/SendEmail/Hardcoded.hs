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
    :: SendEmail.Handle -> Hackathon -> RegisterInfo -> E.UUID -> IO ()
sendRegisterSuccessEmail sendEmail hackathon info uuid = SendEmail.sendEmail
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
    , "    " <> hContactUrl hackathon
    , ""
    , "For various questions, or socializing with other attendees,"
    , "you can join our Slack organisation:"
    , ""
    , "    " <> hSlackUrl hackathon
    , ""
    , "Warm regards"
    , "The " <> hName hackathon <> " Registration Bot"
    ]

sendWaitlistEmail
    :: SendEmail.Handle -> Hackathon -> RegisterInfo -> E.UUID -> IO ()
sendWaitlistEmail sendEmail hackathon info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    (hName hackathon <> ": You're on the waitlist") $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "You have been added to the waitlist for " <> hName hackathon <> "."
    , ""
    , "We will let you know when places become available."
    , ""
    , "You can view your status here:"
    , ""
    , "    " <> hBaseUrl hackathon <> "/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    " <> hContactUrl hackathon
    , ""
    , "Warm regards"
    , "The " <> hName hackathon <> " Registration Bot"
    ]

sendPopWaitlistEmail
    :: SendEmail.Handle -> Hackathon -> RegisterInfo -> E.UUID -> IO ()
sendPopWaitlistEmail sendEmail hackathon info uuid = SendEmail.sendEmail
    sendEmail
    (riEmail info)
    (hName hackathon <> ": You are now registered!") $ T.unlines
    [ "Hello " <> riName info <> ","
    , ""
    , "Great news!  Some places for " <> hName hackathon <> " became available."
    , "You have been removed from the waiting list and are now"
    , "registered to attend " <> hName hackathon <> "."
    , ""
    , "You can view your registration here:"
    , ""
    , "    " <> hBaseUrl hackathon <> "/ticket?uuid=" <> E.uuidToText uuid
    , ""
    , "If you have any concerns, you can find our contact info here:"
    , ""
    , "    " <> hContactUrl hackathon
    , ""
    , "For various questions, or socializing with other attendees,"
    , "you can join our Slack organisation:"
    , ""
    , "    " <> hSlackUrl hackathon
    , ""
    , "Warm regards"
    , "The " <> hName hackathon <> " Registration Bot"
    ]