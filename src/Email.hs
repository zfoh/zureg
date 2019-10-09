import qualified Zureg.Hackathon
import qualified Zureg.Main.Email

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Email.main
