import qualified Zureg.Main.Lambda as Lambda
import qualified ZuriHac2019

main :: IO ()
main = do
    config <- Lambda.loadConfig
    ZuriHac2019.withHandle config (Lambda.main config)
