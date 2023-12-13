{-# LANGUAGE ScopedTypeVariables #-}
import           AWS.Lambda.Runtime      (mRuntime)
import qualified Data.Aeson              as A
import qualified Network.Wai.Handler.Hal as WaiHandler
import           System.Environment      (lookupEnv)
import qualified Zureg.Hackathon
import qualified Zureg.Main.Janitor
import qualified Zureg.Main.Web          as Web

main :: IO ()
main = do
    handler <- lookupEnv "_HANDLER"
    case handler of
        Just "janitor" -> mRuntime $ \(_ :: A.Value) ->
            Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Janitor.main
        _ -> do
            app <- Zureg.Hackathon.withHackathonFromEnv Web.app
            mRuntime $ WaiHandler.run app
