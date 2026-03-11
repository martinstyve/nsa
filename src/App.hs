{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Lucid                    (Html)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid       (HTML)

import           Html
import           PaceRange
import           VDOT

-- | shoutout to servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- | https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26
type API
  = "vdot" :> Capture "time" Double :> Capture "dist" String :> Get '[ HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server time distance = do
  let dist = parseDist distance
  let vdot = calculateVDOT time dist
  let hmTime = secToHMS $ equivalentTime vdot HalfMarathon
  return $ Html.page vdot hmTime
  where
    parseDist "5k"       = FiveK -- TODO: parse
    parseDist "10k"      = TenK
    parseDist "marathon" = Marathon
    parseDist _          = FiveK

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:3000/vdot/1080/FiveK"
  run 3000 app
