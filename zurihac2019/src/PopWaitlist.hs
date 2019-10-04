import qualified Zureg.Main.PopWaitlist as PopWaitlist
import qualified ZuriHac2019

main :: IO ()
main = do
    config <- PopWaitlist.loadConfig
    ZuriHac2019.withHandle config (PopWaitlist.main config)