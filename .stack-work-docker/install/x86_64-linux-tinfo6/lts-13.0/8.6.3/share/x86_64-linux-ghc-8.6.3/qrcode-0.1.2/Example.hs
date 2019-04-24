import Graphics.Pgm
import Data.Maybe

import Codec.Binary.QRCode

main :: IO ()
main = arrayToFile "hello.pgm"
     . toArray
     . fromJust
     . encode (fromJust $ version 1) M Alphanumeric
     $ "hello world"
