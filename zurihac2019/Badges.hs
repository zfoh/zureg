import qualified Zureg.Main.Badges as Badges
import qualified ZuriHac2019

main :: IO ()
main = ZuriHac2019.withHandle undefined Badges.main