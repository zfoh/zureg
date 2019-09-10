import qualified Zureg.Main.Lambda as Lambda
import Data.Proxy (Proxy (..))

main :: IO ()
main = Lambda.main (Proxy :: Proxy ())
