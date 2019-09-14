import qualified Zureg.Main.Email as Email
import qualified ZuriHac2019

main :: IO ()
main = ZuriHac2019.withHandle Email.main
