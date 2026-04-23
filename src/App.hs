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
import           RaceDistance
import           RunTime                  as RT
import           VDOT

newtype TimeParam = TimeParam RT.RunTime

-- https://hackage-content.haskell.org/package/http-api-data-0.7/docs/Web-HttpApiData.html
-- parse "string" in url to a TimeParam
instance FromHttpApiData TimeParam where
  parseUrlPiece :: Text -> Either Text TimeParam
  parseUrlPiece piece =
    case P.parseTime piece of
      Left err      -> Left (P.inputErrorText err)
      Right runTime -> Right (TimeParam runTime)

-- servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26
type API
  = Get '[ HTML] (Html ())
  :<|> "result" :> QueryParam "time" TimeParam :> QueryParam "dist" Text :> QueryParam "customDist" Text
  :> Get '[ HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server = homeHandler :<|> resultHandler

homeHandler :: Handler (Html ())
homeHandler = return Html.index

resultHandler :: Maybe TimeParam -> Maybe Text -> Maybe Text -> Handler (Html ())
resultHandler maybeTime maybeDist maybeCustomDist =
  case validateParams maybeTime maybeDist maybeCustomDist of
    Left maybeError  -> return (Html.indexMaybeError maybeError)
    Right (runTime, raceDistance) -> return (buildResultPage runTime raceDistance)

validateParams :: Maybe TimeParam -> Maybe Text -> Maybe Text -> Either (Maybe Text) (RT.RunTime, RaceDistance)
validateParams (Just (TimeParam runTime)) (Just distChoice) maybeCustomDist =
  case P.resolveDistanceSelection distChoice maybeCustomDist of
    Left err -> Left (Just (P.inputErrorText err))
    Right raceDistance -> Right (runTime, raceDistance)
validateParams _ _ _ = Left Nothing

buildResultPage :: RT.RunTime -> RaceDistance -> Html ()
buildResultPage runTime raceDistance =
  Html.resultPage vdot raceTable intervalPaces
  where
    totalSeconds = fromIntegral (RT.runTimeToSec runTime)
    vdot = calculateVDOT totalSeconds raceDistance
    raceTable =
      [ (presetLabel preset, RT.formatRunTime (equivalentTime vdot (presetDistance preset)))
      | preset <- presetRaceDistances ]
    intervalPaces = calculatePaces vdot

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:6767"
  run 6767 app
