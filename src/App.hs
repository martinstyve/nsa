{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Lucid                    (Html)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid

import           Data.Text                (Text)
import           Text.Megaparsec          (runParser)


import           Html
import           PaceRange
import           VDOT
import Parser

-- servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26

type API = 
    Get '[HTML] (Html ()) 
    :<|> "result" :> QueryParam "time" Text :> QueryParam "dist" Text :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server = homeHandler :<|> calcHandler
  where
    homeHandler = return Html.index

    calcHandler (Just t) (Just d) = do  
      case runParser Parser.timeParser "" t of
        Left _ -> return Html.index
        Right runTime -> do
          let totalSeconds = fromIntegral $ Parser.runTimeToSec runTime
          let dist = parseDist d
          let vdot = calculateVDOT totalSeconds dist
          let hmTime = equivalentTime vdot HalfMarathon
          let formattedHM = Parser.formatRunTime hmTime
          return $ Html.resultPage vdot formattedHM

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
