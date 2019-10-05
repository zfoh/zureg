import qualified Zureg.Main.Badges as Badges
import qualified ZuriHac2019

main :: IO ()
main = do
    config <- Badges.loadConfig
    ZuriHac2019.withHandle config Badges.main