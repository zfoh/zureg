import qualified Zureg.Main.Export as Export
import qualified ZuriHac2019

main :: IO ()
main = do
    config <- Export.loadConfig
    ZuriHac2019.withHandle config (Export.main config)