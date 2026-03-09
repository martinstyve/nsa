{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           PaceRange -- should not be here but who knows
import           Servant
import           VDOT

-- | shoutout to servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- | https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26
type API
  = "vdot" :> Capture "tid" Double :> Capture "dist" String :> Get
      '[ JSON]
      Result

api :: Proxy API
api = Proxy

server :: Server API
server time distance = do
  let dist = parseDist distance
  let vdot = calculateVDOT time dist
  let hmTime = equivalentTime vdot HalfMarathon
  return
    $ Result
        {raceVDOT = vdot, equivHM = secToHMS hmTime, paces = ["fast", "fast:r"]}
  where
    parseDist "5k"       = FiveK -- TODO: megaparsec module
    parseDist "10k"      = TenK
    parseDist "marathon" = Marathon
    parseDist _          = FiveK -- catch

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:3000/vdot/1080/FiveK"
  run 3000 app
