import qualified Zureg.Hackathon
import qualified Zureg.Main.Web

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Web.main
