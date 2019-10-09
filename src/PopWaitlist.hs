import qualified Zureg.Hackathon
import qualified Zureg.Main.PopWaitlist

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.PopWaitlist.main
