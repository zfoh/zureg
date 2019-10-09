import qualified Zureg.Hackathon
import qualified Zureg.Main.Export

main :: IO ()
main = Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Export.main
