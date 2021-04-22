-- | Simple abstraction over lambda + API gateway.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Serverless
    ( Request (..)
    , requestPath
    , requestLookupQueryStringParameter

    , Response (..)
    , response
    , response200
    , response302
    , responseHtml

    , ServerlessException (..)
    , main

    , runForm
    ) where

import           Control.Exception      (Exception, fromException, throwIO)
import qualified Data.Aeson             as A
import qualified Data.Aeson.TH.Extended as A
import qualified Data.HashMap.Strict    as HMS
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.URLEncoded        as UrlEncoded
import qualified Data.Vector            as V
import qualified System.IO              as IO
import qualified Text.Digestive         as D
import qualified Zureg.Lambda           as Lambda

data Request = Request
    { reqHttpMethod            :: !T.Text
    , reqPath                  :: !T.Text
    , reqQueryStringParameters :: !(Maybe (HMS.HashMap T.Text A.Value))
    , reqBody                  :: !(Maybe T.Text)
    } deriving (Show)

requestPath :: Request -> [T.Text]
requestPath = filter (not . T.null) . T.splitOn "/" . reqPath

requestLookupQueryStringParameter :: T.Text -> Request -> Maybe T.Text
requestLookupQueryStringParameter k req = do
    params <- reqQueryStringParameters req
    val    <- HMS.lookup k params
    case val of
        A.Array xs | not (V.null xs), A.String t <- V.head xs -> Just t
        A.String t                                            -> Just t
        _                                                     -> Nothing

data Response = Response
    { rspStatusCode :: !Int
    , rspBody       :: !TL.Text
    , rspHeaders    :: !(HMS.HashMap T.Text T.Text)
    } deriving (Show)

response :: Int -> TL.Text -> Response
response c b = Response c b HMS.empty

response200 :: TL.Text -> Response
response200 = response 200

response302 :: T.Text -> Response
response302 loc = Response
    { rspStatusCode = 302
    , rspBody       = ""
    , rspHeaders    = HMS.singleton "Location" loc
    }

responseHtml :: Response -> Response
responseHtml rsp = rsp
    { rspHeaders = HMS.insert "Content-Type" "text/html; charset=utf-8"
        (rspHeaders rsp)
    }

data ServerlessException = ServerlessException Int String deriving (Show)

instance Exception ServerlessException

$(A.deriveJSON A.options ''Request)
$(A.deriveJSON A.options ''Response)

main :: IO.Handle -> IO.Handle -> (Request -> IO Response) -> IO ()
main ih oh f =
    -- We're really just wrapping 'Lambda.main' with more specific
    -- request/response types and error handling.
    Lambda.main ih oh errorResponse $ \req -> do
        T.hPutStrLn IO.stderr $ reqHttpMethod req <> " " <> reqPath req
        f req
  where
    errorResponse exception
        | Just (ServerlessException c m) <- fromException exception =
            responseHtml $ response c (TL.pack m)
        | Just (Lambda.ParseInputJsonException err) <- fromException exception =
            responseHtml $ response 400 $ TL.pack $
            "Could not parse request: " ++ err
        | otherwise = responseHtml $ response 500 (TL.pack (show exception))

runForm
    :: Request -> T.Text -> D.Form v IO a -> IO (D.View v, Maybe a)
runForm Request {..} name form
    | reqHttpMethod == "GET" = do
        view <- D.getForm name form
        return (view, Nothing)

    | reqHttpMethod == "POST" = do
        body    <- maybe (fail "missing post body") return reqBody
        encoded <- UrlEncoded.importString $ T.unpack body

        let env :: D.Env IO
            env = \path -> return $
                map (D.TextInput . T.pack) $
                UrlEncoded.lookupAll (T.unpack $ D.fromPath path) encoded

        D.postForm name form $ \_ -> return env

    | otherwise = throwIO $ ServerlessException 400 $
        "Expected or GET POST"
