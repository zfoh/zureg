import qualified Zureg.Hackathon
import qualified Zureg.Main.Badges

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Badges.main
