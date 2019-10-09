import qualified Zureg.Hackathon
import qualified Zureg.Main.Lambda

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Lambda.main
