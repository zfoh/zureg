{-# LANGUAGE OverloadedStrings #-}
module Zureg.Http
    ( html
    , redirect

    , HttpException (..)
    , httpExceptionMiddleware

    , runForm
    ) where

import           Control.Exception             (Exception, throwIO, try)
import qualified Data.IORef                    as IORef
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.URLEncoded               as UrlEncoded
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai                   as Wai
import qualified Text.Blaze.Html.Renderer.Utf8 as RenderHtml
import qualified Text.Blaze.Html5              as H
import qualified Text.Digestive                as D

html :: H.Html -> Wai.Response
html = Wai.responseBuilder Http.status200 headers . RenderHtml.renderHtmlBuilder
  where
    headers = [("Content-Type", "text/html; charset=utf-8")]

redirect :: T.Text -> Wai.Response
redirect l = Wai.responseLBS Http.status302 [("Location", T.encodeUtf8 l)] ""

data HttpException = HttpException Int String deriving (Show)

instance Exception HttpException

httpExceptionMiddleware :: Wai.Middleware
httpExceptionMiddleware app req respond = do
    responded <- IORef.newIORef False
    errOrHttpException <- try $ app req $ \response -> do
        IORef.writeIORef responded True
        respond response
    alreadyResponded <- IORef.readIORef responded
    case errOrHttpException of
        Left (HttpException code msg) | not alreadyResponded ->
            let title = "Error " ++ show code in
            respond $
                Wai.mapResponseStatus (\_ ->
                    Http.mkStatus code (T.encodeUtf8 $ T.pack title)) $
                html $ H.toHtml $ title ++ ": " ++ msg
        Left err -> throwIO err
        Right result -> pure result

runForm
    :: Wai.Request -> TL.Text -> T.Text -> D.Form v IO a
    -> IO (D.View v, Maybe a)
runForm req reqBody name form
    | Wai.requestMethod req == Http.methodGet = do
        view <- D.getForm name form
        return (view, Nothing)

    | Wai.requestMethod req == Http.methodPost = do
        encoded <- UrlEncoded.importString $ TL.unpack reqBody

        let env :: D.Env IO
            env = \path -> return $
                map (D.TextInput . T.pack) $
                UrlEncoded.lookupAll (T.unpack $ D.fromPath path) encoded

        D.postForm name form $ \_ -> return env

    | otherwise = throwIO $ HttpException 400 $
        "Expected or GET POST"
