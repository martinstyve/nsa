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

data AppError
  = MissingRequiredInput
  | InputParseError InputError
  | VdotCalculationError VDOTError

appErrorText :: AppError -> Maybe Text
appErrorText MissingRequiredInput = Nothing
appErrorText (InputParseError err) = Just (P.inputErrorText err)
appErrorText (VdotCalculationError err) = Just (vdotErrorText err)

newtype TimeParam = TimeParam Text

instance FromHttpApiData TimeParam where
  parseUrlPiece :: Text -> Either Text TimeParam
  parseUrlPiece = Right . TimeParam

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
    Left err -> return (Html.indexMaybeError (appErrorText err))
    Right (runTime, raceDistance) ->
      case buildResultPage runTime raceDistance of
        Left err -> return (Html.indexMaybeError (appErrorText err))
        Right page -> return page

validateParams :: Maybe TimeParam -> Maybe Text -> Maybe Text -> Either AppError (RT.RunTime, RaceDistance)
validateParams (Just (TimeParam timeText)) (Just distChoice) maybeCustomDist =
  case P.parseTime timeText of
    Left err -> Left (InputParseError err)
    Right runTime ->
      case P.resolveDistanceSelection distChoice maybeCustomDist of
        Left err -> Left (InputParseError err)
        Right raceDistance -> Right (runTime, raceDistance)
validateParams _ _ _ = Left MissingRequiredInput

buildResultPage :: RT.RunTime -> RaceDistance -> Either AppError (Html ())
buildResultPage runTime raceDistance =
  case raceTableOrError of
    Left err -> Left (VdotCalculationError err)
    Right raceTable -> Right (Html.resultPage vdot raceTable intervalPaces)
  where
    totalSeconds = fromIntegral (RT.runTimeToSec runTime)
    vdot = calculateVDOT totalSeconds raceDistance
    raceTableOrError = sequence
      [ case equivalentTime vdot (presetDistance preset) of
          Left err -> Left err
          Right time -> Right (presetLabel preset, RT.formatRunTime time)
      | preset <- presetRaceDistances ]
    intervalPaces = calculatePaces vdot

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:6767"
  run 6767 app
