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

-- servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26

type API = 
    Get '[HTML] (Html ()) 
    :<|> "result" :> QueryParam "time" Double :> QueryParam "dist" String :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server = homeHandler :<|> calcHandler
  where
    homeHandler = return Html.index

    calcHandler (Just t) (Just d) = do  
      let dist = parseDist d
      let vdot = calculateVDOT t dist
      let hmTime = secToHMS $ equivalentTime vdot HalfMarathon
      return $ Html.resultPage vdot hmTime
    calcHandler _ _ = return Html.index

    parseDist "5k"       = FiveK -- TODO: parse
    parseDist "10k"      = TenK
    parseDist "marathon" = Marathon
    parseDist _          = FiveK

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:3000"
  run 3000 app
