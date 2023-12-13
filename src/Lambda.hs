import           AWS.Lambda.Runtime      (mRuntime)
import qualified Network.Wai.Handler.Hal as WaiHandler
import           System.Environment      (lookupEnv)
import qualified Zureg.Hackathon
import qualified Zureg.Main.Janitor      as Janitor
import qualified Zureg.Main.Web          as Web

main :: IO ()
main = do
    handler <- lookupEnv "_HANDLER"
    case handler of
        Just "janitor" ->
            Zureg.Hackathon.withHackathonFromEnv $ mRuntime . Janitor.app
        Just "web" -> do
            app <- Zureg.Hackathon.withHackathonFromEnv Web.app
            mRuntime $ WaiHandler.run app
        _ -> fail "_HANDLER not set"
