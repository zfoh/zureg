import qualified Zureg.Hackathon
import qualified Zureg.Main.Janitor

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Janitor.main
