{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Data.Text                (Text)
import           Lucid                    (Html)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid

import           Html
import           PaceRange
import           Parser                   as P
import           RunTime                  as RT
import           VDOT

newtype TimeParam = TimeParam RT.RunTime

newtype DistanceParam = DistanceParam RaceDistance

-- https://hackage-content.haskell.org/package/http-api-data-0.7/docs/Web-HttpApiData.html
-- parse "string" in url to a TimeParam
instance FromHttpApiData TimeParam where
  parseUrlPiece :: Text -> Either Text TimeParam
  parseUrlPiece piece =
    case P.parseTime piece of
      Left err      -> Left (P.inputErrorText err)
      Right runTime -> Right (TimeParam runTime)

instance FromHttpApiData DistanceParam where
  parseUrlPiece :: Text -> Either Text DistanceParam
  parseUrlPiece piece =
    case P.parseDistance piece of
      Left err           -> Left (P.inputErrorText err)
      Right raceDistance -> Right (DistanceParam raceDistance)

-- servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26
type API
  = Get '[ HTML] (Html ())
  :<|> "result" :> QueryParam "time" TimeParam :> QueryParam "dist" DistanceParam
  :> Get '[ HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server = homeHandler :<|> calcHandler
  where
    homeHandler = return Html.index
    calcHandler (Just (TimeParam runTime)) (Just (DistanceParam raceDistance)) = do
      let totalSeconds = fromIntegral $ RT.runTimeToSec runTime -- todo toSec be double or ...
      let vdot = calculateVDOT totalSeconds raceDistance -- force calculateVDOT to be integer, trouble bisect function?
      let raceTable =
            [ ("5k", RT.formatRunTime (equivalentTime vdot FiveK))
            , ("10k", RT.formatRunTime (equivalentTime vdot TenK))
            , ("half", RT.formatRunTime (equivalentTime vdot HalfMarathon))
            , ("marathon", RT.formatRunTime (equivalentTime vdot Marathon))
            ]
      let intervalPaces = calculatePaces vdot
      return $ Html.resultPage vdot raceTable intervalPaces
    calcHandler _ _ = return Html.index -- fallback

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:6767"
  run 6767 app
