{-# LANGUAGE ScopedTypeVariables #-}
import           AWS.Lambda.Runtime (mRuntime)
import qualified Data.Aeson         as A
import qualified Zureg.Hackathon
import qualified Zureg.Main.Janitor

main :: IO ()
main = mRuntime $ \(_ :: A.Value) ->
    Zureg.Hackathon.withHackathonFromEnv Zureg.Main.Janitor.main
