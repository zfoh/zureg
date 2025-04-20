-- | HTML responses.
{-# LANGUAGE LambdaCase        #-}
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

import qualified Codec.QRCode                as QRCode
import qualified Codec.QRCode.JuicyPixels    as QRCode.JP
import           Control.Monad               (unless, when)
import qualified Data.ByteString             as B
import qualified Data.FileEmbed              as Embed
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.UUID                   (UUID)
import qualified Data.UUID                   as UUID
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Zureg.Captcha               as Captcha
import           Zureg.Database.Models
import qualified Zureg.Form                  as Form
import qualified Zureg.Hackathon             as Hackathon
import           Zureg.Hackathon             (Hackathon)
import           Zureg.Main.Badges           (Badge (..), registrantToBadge)
import qualified Zureg.Main.Export as Export

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

register :: Hackathon -> Captcha.ClientHtml -> D.View H.Html -> H.Html
register hackathon captchaHtml view =
    template (Captcha.chScript captchaHtml) $
    Form.registerView hackathon captchaHtml view

registerSuccess :: Registration -> H.Html
registerSuccess Registration {..} = template mempty $ do
    H.h1 "Registration successful"
    H.p $ H.toHtml rName <> ", your registration was successful."
    H.p $ "You will receive a confirmation mail at " <> H.toHtml rEmail <>
        " soon."

registerWaitlist :: Registration -> H.Html
registerWaitlist Registration {..} = template mempty $ do
    H.h1 "You are now on the waitlist"
    H.p $ H.toHtml rName <> ", your have been added to the waitlist."
    H.p $ "You will receive an email at " <> H.toHtml rEmail <> " soon."

ticket :: Hackathon -> Registration -> H.Html
ticket hackathon registration@Registration {..} = template
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
        when (rState == Confirmed) $
            qrimg $ T.unpack $ UUID.toText rID

        H.div $ do
            H.h1 $ fst $ registrationState rState
            registrantInfo registration  -- TODO: inline?

        when (rState == Cancelled) $
            H.form H.! A.method "GET" H.! A.action "register" $ do
                H.input H.! A.type_ "submit"
                    H.! A.value "Take me back to the registration"

        when (Hackathon.confirmation hackathon && rState == Registered) $ do
            H.p "Please confirm your registration so we can get an accurate count of attendees for food, etc."
            H.form H.! A.method "GET" H.! A.action "confirm" $ do
                H.input H.! A.type_ "hidden" H.! A.name "uuid"
                    H.! A.value (H.toValue (UUID.toText rID))
                H.input H.! A.type_ "submit"
                    H.! A.value "Confirm my registration and access ticket"

        when (registrantCanJoinChat rState) $ do
            H.toHtml $ Hackathon.name hackathon
            " uses Discord as a chat platform for coordination."
            "You can join the Discord server here:"
            H.form H.! A.method "GET" H.! A.action "chat" $ do
                H.input H.! A.type_ "hidden" H.! A.name "uuid"
                    H.! A.value (H.toValue (UUID.toText rID))
                H.input H.! A.type_ "submit"
                    H.! A.value "Generate Discord invite"

        unless (rState == Cancelled) $ do
            H.p $ do
                "If you can no longer attend "
                H.toHtml (Hackathon.name hackathon) <> ", we ask that you "
                "cancel your registration because space is limited."
            H.form H.! A.method "GET" H.! A.action "cancel" $ do
                H.input H.! A.type_ "hidden" H.! A.name "uuid"
                    H.! A.value (H.toValue (UUID.toText rID))
                H.input H.! A.type_ "submit"
                    H.! A.value "Cancel my registration")

registrationState :: RegistrationState -> (H.Html, Bool)
registrationState rs = case rs of
    Cancelled  -> ("❌ Cancelled", False)
    Registered -> ("✅ Registered", True)
    Confirmed  -> ("✅ Confirmed", True)
    Waitlisted -> ("⌛ on the waitlist", False)
    Spam       -> ("🥫 Spam", False)

registrantInfo :: Registration -> H.Html
registrantInfo Registration {..} = H.p $ do
    H.strong (H.toHtml rName ) <> H.br
    H.toHtml rEmail <> H.br

cancel :: Maybe UUID -> D.View H.Html -> H.Html
cancel mbUuid view = template mempty $
    H.div H.! A.class_ "danger" $ Form.cancelView mbUuid view

cancelSuccess :: H.Html
cancelSuccess = template mempty $ do
    H.p "You have successfully cancelled your registration."
    H.p "We hope to see you next year!"

qrimg :: String -> H.Html
qrimg payload = fromMaybe "Could not generate QR code" $ do
    img <- QRCode.encode options encoding payload
    let image = QRCode.JP.toPngDataUrlT border scale img
    return $ H.img H.! A.class_ "qr" H.! A.src (H.toValue image)
  where
    border   = 1
    scale    = 16
    encoding = QRCode.Iso8859_1
    options  = (QRCode.defaultQRCodeOptions QRCode.L)
        { QRCode.qroMinVersion = 2
        }

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

scan :: Hackathon -> Registration -> H.Html
scan hackathon registrant@Registration {..} = H.ul $ do
    H.li $ H.strong $
        let (html, ok) = registrationState rState in (if ok then id else red) html

    when rVip $ H.li $ "⭐ " <> H.strong "VIP"

    H.li $ case registrantToBadge (Export.fromModel registrant Nothing) of
        Nothing            -> red "No Badge"
        Just (Badge badge) -> "Badge: " <> H.strong (H.toHtml badge)

    H.li $ case rTShirtSize of
        Nothing   -> "No T-Shirt"
        Just size -> case rRegisteredAt of
            at | maybe False (at >=) (Hackathon.tShirtDeadline hackathon) ->
                H.p $ H.strong "Pick up T-Shirt later"
            _ -> do
                "T-Shirt: "
                H.strong $ H.toHtml (show size)
  where
    red x = H.span H.! A.style "color: #aa0000" $ x
