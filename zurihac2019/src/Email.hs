import qualified Zureg.Main.Email as Email
import qualified ZuriHac2019

main :: IO ()
main = do
    config <- Email.loadConfig
    ZuriHac2019.withHandle config (Email.main config)
