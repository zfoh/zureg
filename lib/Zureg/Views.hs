-- | HTML responses.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Views
    ( register
    , registerSuccess
    , registerWaitlist

    , ticket

    , cancel
    , cancelSuccess

    , scanner

    , scan
    ) where

import qualified Codec.Binary.QRCode         as QRCode
import qualified Codec.Picture               as JP
import           Control.Monad               (unless, when)
import qualified Data.Array                  as Array
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.FileEmbed              as Embed
import           Data.Maybe                  (fromMaybe)
import           Data.List                   (intercalate)
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import qualified Eventful                    as E
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Zureg.Form                  as Form
import           Zureg.Model
import qualified Zureg.ReCaptcha             as ReCaptcha

template :: H.Html -> H.Html -> H.Html
template head' body = H.docTypeHtml $ do
    H.head $ do
        H.title "Registration"
        H.meta
            H.! A.name "viewport"
            H.! A.content "width=device-width, initial-scale=1"

        H.style $ do
            "body {"
            "    font-family: Helvetica neue, Helvetica, sans-serif;"
            "    font-size: 16px;"
            "    max-width: 800px;"
            "    padding: 10px;"
            "}"
            "label {"
            "    display: block;"
            "}"
            "input[type=\"submit\"]:hover { background: #efefef; }"
            "input, select {"
            "    width: 100%;"
            "    margin: 6px auto 20px auto;"
            "    font-size: 20px;"
            "    display: block;"
            "    border: 1px solid #555;"
            "}"
            "input.checkbox {"
            "    display: inline;"
            "    margin: 0px 10px 0px 0px;"
            "    width: inherit;"
            "    float: left;"
            --   Double-sized Checkboxes
            "    -ms-transform: scale(2);"      -- IE
            "    -moz-transform: scale(2);"     -- FF
            "    -webkit-transform: scale(2);"  -- Safari and Chrome
            "    -o-transform: scale(2);"       -- Opera
            "}"
            "div.g-recaptcha {"
            "    margin-bottom: 20px;"
            "}"
            ".errors {"
            "    font-weight: bold;"
            "    color: red;"
            "}"
            "h1, h2, h3 {"
            "    color: #258bcd;"
            "    font-weight: 700;"
            "}"
            "div.danger {"
            "    margin: 20px 0px 20px 0px;"
            "    border: 5px solid #258bcd;"
            "    padding: 20px 20px 0px 20px;"
            "}"
        head'
    H.body body

register :: ReCaptcha.ClientHtml -> D.View H.Html -> H.Html
register recaptcha view =
    template (ReCaptcha.chScript recaptcha) $
    Form.registerView recaptcha view

registerSuccess :: E.UUID -> RegisterInfo -> H.Html
registerSuccess _uuid RegisterInfo {..} = template mempty $ do
    H.h1 "Registration successful"
    H.p $ H.toHtml riName <> ", your registration was successful."
    H.p $ "You will receive a confirmation mail at " <> H.toHtml riEmail <>
        " soon."

registerWaitlist :: E.UUID -> RegisterInfo -> H.Html
registerWaitlist _uuid RegisterInfo {..} = template mempty $ do
    H.h1 "You are now on the waitlist"
    H.p $ H.toHtml riName <> ", your have been added to the waitlist."
    H.p $ "You will receive an email at " <> H.toHtml riEmail <> " soon."

ticket :: Registrant -> H.Html
ticket r@Registrant {..} = template
    (H.style $ do
        "img.qr {"
        "  display: block;"
        "  width: 400px;"
        "  max-width: 50%;"
        "  image-rendering: optimizeSpeed;"
        "  image-rendering: -moz-crisp-edges;"
        "  image-rendering: -o-crisp-edges;"
        "  image-rendering: -webkit-optimize-contrast;"
        "  image-rendering: pixelated;"
        "  image-rendering: optimize-contrast;"
        "  -ms-interpolation-mode: nearest-neighbor;"
        "}")
    (do
        when (rState == Just Confirmed) $
            qrimg $ T.unpack $ E.uuidToText rUuid

        registrantInfo r

        case rInfo of
            Nothing                -> mempty
            Just RegisterInfo {..} -> case riRegisteredAt of 
                Nothing            -> mempty
                Just rRegisteredAt -> when(rRegisteredAt >= Time.UTCTime (Time.fromGregorian 2019 5 7) (15*60*60)) $
                    H.p $ do 
                      "Please note that we have ordered the T-Shirts" 
                      " and cannot guarantee that you will receive one if you register at this time."

        when (rState == Just Cancelled) $
            H.form H.! A.method "GET" H.! A.action "register" $ do
                H.input H.! A.type_ "submit"
                    H.! A.value "Take me back to the registration"

        when (rState == Just Registered) $
            H.form H.! A.method "GET" H.! A.action "confirm" $ do
                H.input H.! A.type_ "hidden" H.! A.name "uuid"
                    H.! A.value (H.toValue (E.uuidToText rUuid))
                H.input H.! A.type_ "submit"
                    H.! A.value "Confirm my registration and access ticket"

        unless (rState == Just Cancelled) $
            H.form H.! A.method "GET" H.! A.action "cancel" $ do
                H.input H.! A.type_ "hidden" H.! A.name "uuid"
                    H.! A.value (H.toValue (E.uuidToText rUuid))
                H.input H.! A.type_ "submit"
                    H.! A.value "Cancel my registration")

registrantInfo :: Registrant -> H.Html
registrantInfo Registrant {..} = H.div $ do
    H.h1 $ case rState of
        Nothing         -> "❌ Not registered"
        Just Cancelled  -> "❌ Cancelled"
        Just Registered -> "✅ Registered"
        Just Confirmed  -> "✅ Confirmed"
        Just Waitlisted -> "⌛ on the waitlist"
    case rInfo of
        Just RegisterInfo {..} -> H.p $ do
            H.strong (H.toHtml riName ) <> H.br
            H.toHtml riEmail <> H.br
            case riTShirt of 
                Just rTShirt -> 
                    "T-Shirt: " 
                    <> (H.toHtml.show $ fst rTShirt) <> ", "
                    <> (H.toHtml.show $ snd rTShirt)
                    <>  case riMentor of
                            True -> ", Mentorshirt"  <> H.br
                            False -> H.br
                Nothing -> mempty
            "Track interest(s): " 
            H.toHtml $ intercalate ", " $
                ["Beginner" | tiBeginner tiTrackInterest] 
                ++ ["Intermediate" | tiIntermediate tiTrackInterest]
                ++ ["Advanced" | tiAdvanced tiTrackInterest]
                ++ ["GHC DevOps" | tiGhcDevOps tiTrackInterest] 
        Nothing ->
            mempty

cancel :: Maybe E.UUID -> D.View H.Html -> H.Html
cancel mbUuid view = template mempty $
    H.div H.! A.class_ "danger" $ Form.cancelView mbUuid view

cancelSuccess :: H.Html
cancelSuccess = template mempty $ do
    H.p "You have successfully cancelled your registration."
    H.p "We hope to see you next year!"

qrimg :: String -> H.Html
qrimg qrdata = fromMaybe "Could not generate QR code" $ do
    version <- QRCode.version 2
    matrix  <- QRCode.encode version QRCode.L QRCode.Alphanumeric qrdata
    let array = QRCode.toArray matrix :: Array.Array (Int, Int) JP.Pixel8
        image = JP.generateImage
            (\x y -> array Array.! (x, y))
            (QRCode.width matrix + 1)
            (QRCode.width matrix + 1)

        base64 = Base64.encode $ JP.encodePng image
        src    = "data:image/png;base64," <> H.unsafeLazyByteStringValue base64

    return $ H.img
        H.! A.class_ "qr"
        H.! A.src src

scanner :: H.Html
scanner = H.docTypeHtml $ H.body $ do
    H.script H.! A.type_ "text/JavaScript" $
        H.unsafeByteString fileJsQr
    H.script H.! A.type_ "text/JavaScript" $
        H.unsafeByteString fileScanner
    H.script H.! A.type_ "text/JavaScript" $ "scanner();"

fileJsQr :: B.ByteString
fileJsQr = $(Embed.embedFile "static/jsQR-807b073.js")

fileScanner :: B.ByteString
fileScanner = $(Embed.embedFile "static/scanner.js")

scan :: Registrant -> H.Html
scan registrant = registrantInfo registrant <> "Pick up T-Shirt later"
