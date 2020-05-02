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
import qualified Data.Text                   as T
import qualified Eventful                    as E
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Zureg.Form                  as Form
import           Zureg.Hackathon             (Hackathon)
import qualified Zureg.Hackathon             as Hackathon
import           Zureg.Main.Badges           (previewBadge, registrantToBadge)
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
            "    width: calc(100% - 30px);"
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

register :: Hackathon e a -> ReCaptcha.ClientHtml -> D.View H.Html -> H.Html
register hackathon recaptcha view =
    template (ReCaptcha.chScript recaptcha) $
    Form.registerView hackathon recaptcha view

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

ticket :: Hackathon e a -> Registrant a -> H.Html
ticket hackathon Registrant {..} = template
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

        H.div $ do
            H.h1 $ registerState rState
            whenJust rInfo $ registrantInfo
            whenJust rAdditionalInfo $ Hackathon.ticketView hackathon

        when (rState == Just Cancelled) $
            H.form H.! A.method "GET" H.! A.action "register" $ do
                H.input H.! A.type_ "submit"
                    H.! A.value "Take me back to the registration"

        when (Hackathon.confirmation hackathon && rState == Just Registered) $
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

registerState :: Maybe RegisterState -> H.Html
registerState rs = case rs of
    Nothing         -> "❌ Not registered"
    Just Cancelled  -> "❌ Cancelled"
    Just Registered -> "✅ Registered"
    Just Confirmed  -> "✅ Confirmed"
    Just Waitlisted -> "⌛ on the waitlist"

registrantInfo :: RegisterInfo -> H.Html
registrantInfo RegisterInfo {..} = H.p $ do
    H.strong (H.toHtml riName ) <> H.br
    H.toHtml riEmail <> H.br

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
        moduleSize = 16 -- QR-Code version 2 has 25x25 modules, so this amounts to an image size of 400px
        image = JP.generateImage
            (\x y -> array Array.! (x `div` moduleSize, y `div` moduleSize))
            (moduleSize * (QRCode.width matrix + 1))
            (moduleSize * (QRCode.width matrix + 1))

        base64 = Base64.encode $ JP.encodePng image
        src    = "data:image/png;base64," <> H.unsafeLazyByteStringValue base64

    return $ H.img
        H.! A.class_ "qr"
        H.! A.src src

scanner :: H.Html
scanner = H.docTypeHtml $ do
    H.head $ do
        H.meta
            H.! A.name "viewport"
            H.! A.content "width=device-width, initial-scale=1"
        H.style $ do
            "html {"
            "    font-size: 18px;"
            "    font-family: sans-serif;"
            "}"

    H.body $ do
        H.script H.! A.type_ "text/JavaScript" $
            H.unsafeByteString fileJsQr
        H.script H.! A.type_ "text/JavaScript" $
            H.unsafeByteString fileScanner
        H.script H.! A.type_ "text/JavaScript" $ "scanner();"

fileJsQr :: B.ByteString
fileJsQr =
    $(Embed.makeRelativeToProject "static/jsQR-807b073.js" >>= Embed.embedFile)

fileScanner :: B.ByteString
fileScanner =
    $(Embed.makeRelativeToProject "static/scanner.js" >>= Embed.embedFile)

scan :: Hackathon e a -> Registrant a -> H.Html
scan hackathon registrant@Registrant {..} = H.ul $ do
    H.li $ H.strong $ case rState of
        Nothing         -> red "❌ Not registered"
        Just Cancelled  -> red "❌ Cancelled"
        Just Registered -> "✅ Registered"
        Just Confirmed  -> "✅ Confirmed"
        Just Waitlisted -> red "⌛ on the waitlist"

    H.li $ case (registrantRegisteredAt registrant, registrantToBadge registrant) of
        (_, Nothing)                        -> red "No Badge"
        (_, Just badge)                     ->
            "Badge: " <> H.strong (H.toHtml $ previewBadge badge)

    whenJust rAdditionalInfo $ \ri -> H.li (Hackathon.scanView hackathon ri)

  where
    red x = H.span H.! A.style "color: #aa0000" $ x

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust a f = maybe (pure ()) f a
