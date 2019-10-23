-- | Low-ish level module to talk to AWS Lambda.  A Python process spawns a
-- binary and sends events to us; one line of JSON at a time.  We respond by
-- writing back JSON.
module Zureg.Lambda
    ( LambdaException (..)
    , main
    ) where

import qualified Control.Concurrent.Async   as Async
import           Control.Exception          (Exception, SomeException, throwIO)
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified System.IO                  as IO

data LambdaException
    = ParseInputJsonException String

instance Show LambdaException where
    show (ParseInputJsonException str) = "Could not parse input JSON: " ++ str

instance Exception LambdaException

main
    :: (A.FromJSON request, A.ToJSON response)
    => IO.Handle
    -- ^ Input handle, usually stdin
    -> IO.Handle
    -- ^ Output handle, usually stdout
    -> (SomeException -> response)
    -- ^ Generate an error response, used on exceptions
    -> (request -> IO response)
    -- ^ Generate a normal response
    -> IO ()
main ih oh errorResponse normalResponse =
    loop
  where
    loop = do
        eof <- IO.hIsEOF ih
        if eof
            then return ()
            else do
                input <- B.hGetLine ih
                output <- respond input
                BL.hPutStr oh output
                BL8.hPutStrLn oh BL8.empty
                IO.hFlush oh
                loop

    respond :: B.ByteString -> IO BL.ByteString
    respond input = do
        thread <- Async.async $ case A.eitherDecode' (BL.fromChunks [input]) of
            Right x  -> normalResponse x
            Left err -> throwIO $ ParseInputJsonException err

        errOrRsp <- Async.waitCatch thread
        case errOrRsp of
            Right r -> return $ A.encode r
            Left  e -> return $ A.encode $ errorResponse e
