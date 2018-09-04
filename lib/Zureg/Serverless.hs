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
    , responseHtml

    , ServerlessException (..)
    , main

    , runForm
    ) where

import qualified Control.Concurrent.Async   as Async
import           Control.Exception          (Exception, fromException, throwIO)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.TH.Extended     as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict        as HMS
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.URLEncoded            as UrlEncoded
import qualified System.IO                  as IO
import qualified Text.Digestive             as D

data Request = Request
    { reqHttpMethod            :: !T.Text
    , reqPath                  :: !T.Text
    , reqQueryStringParameters :: !(Maybe (HMS.HashMap T.Text T.Text))
    , reqBody                  :: !(Maybe T.Text)
    } deriving (Show)

requestPath :: Request -> [T.Text]
requestPath = filter (not . T.null) . T.splitOn "/" . reqPath

requestLookupQueryStringParameter :: T.Text -> Request -> Maybe T.Text
requestLookupQueryStringParameter k req = do
    params <- reqQueryStringParameters req
    HMS.lookup k params

data Response = Response
    { rspStatusCode :: !Int
    , rspBody       :: !TL.Text
    , rspHeaders    :: !(HMS.HashMap T.Text T.Text)
    } deriving (Show)

response :: Int -> TL.Text -> Response
response c b = Response c b HMS.empty

response200 :: TL.Text -> Response
response200 = response 200

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
    loop
  where
    loop = do
        eof <- IO.hIsEOF ih
        if eof
            then return ()
            else do
                input <- B.hGetLine ih
                output <- respond input
                BL8.hPutStrLn oh output
                IO.hFlush oh
                loop

    respond :: B.ByteString -> IO BL.ByteString
    respond input = do
        thread <- Async.async $ do
            case A.eitherDecode' (BL.fromChunks [input]) of
                Right x  -> f x
                Left err -> throwIO $ ServerlessException 400 $
                    "Could not parse request JSON: " ++ show err

        errOrRsp <- Async.waitCatch thread
        case errOrRsp of
            Right r -> return $ A.encode r
            Left  e -> return $ A.encode $ responseHtml $ case fromException e of
                Just (ServerlessException c m) -> response c (TL.pack m)
                _ -> response 500 (TL.pack (show e))

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
