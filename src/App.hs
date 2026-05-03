{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-| Module      : App
Description : Serve the calculator web application

Defines the Servant API, request validation, and page rendering flow for
the web app
-}
module App where

import           Data.Text                (Text)
import           Lucid                    (Html)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.HTML.Lucid       (HTML)

import           Html                     (index, indexMaybeError, resultPage)
import           PaceRange                (calculatePaces)
import           RunTime                  as RT ( RunTime, runTimeToSec, secToRunTime )
import           Parser                   as P
import           RaceDistance             as RD
import           VDOT

-- | Application-level errors for request handling and result generation
--
-- Constructors:
--
-- * @MissingRequiredInput@ - one or more required query parameters are missing
-- * @InputParseError@ - a query parameter failed validation
-- * @VdotCalculationError@ - the calculator could not produce a result table
data AppError
  = MissingRequiredInput
  | InputParseError InputError
  | VdotCalculationError VDOTError

-- | Convert an application error to display text
appErrorText :: AppError -> Maybe Text
appErrorText MissingRequiredInput       = Nothing
appErrorText (InputParseError err)      = Just (P.inputErrorText err)
appErrorText (VdotCalculationError err) = Just (vdotErrorText err)

-- | Query parameter wrapper for the time input
newtype TimeParam =
  TimeParam Text

instance FromHttpApiData TimeParam where
  parseUrlPiece :: Text -> Either Text TimeParam
  parseUrlPiece = Right . TimeParam

-- | Query parameter wrapper for the selected race distance
newtype DistanceParam =
  DistanceParam Text

instance FromHttpApiData DistanceParam where
  parseUrlPiece :: Text -> Either Text DistanceParam
  parseUrlPiece = Right . DistanceParam

-- | Query parameter wrapper for the optional custom distance value
newtype CustomDistanceParam =
  CustomDistanceParam Text

instance FromHttpApiData CustomDistanceParam where
  parseUrlPiece :: Text -> Either Text CustomDistanceParam
  parseUrlPiece = Right . CustomDistanceParam

-- | Servant API for the calculator home page and result page
type API
  = Get '[ HTML] (Html ()) :<|> "result" :> QueryParam "time" TimeParam :> QueryParam
      "dist" DistanceParam :> QueryParam "customDist" CustomDistanceParam :> Get '[ HTML] (Html ())

-- | Proxy value for the application API
api :: Proxy API
api = Proxy

-- | Application server implementation
server :: Server API
server = homeHandler :<|> resultHandler

-- | Render the home page
homeHandler :: Handler (Html ())
homeHandler = return Html.index

-- | Validate query parameters and render either the result page or an error page
resultHandler :: Maybe TimeParam -> Maybe DistanceParam -> Maybe CustomDistanceParam -> Handler (Html ())
resultHandler maybeTime maybeDist maybeCustomDist =
  case validateParams maybeTime maybeDist maybeCustomDist of
    Left err -> return (Html.indexMaybeError (appErrorText err))
    Right (runTime, raceDistance) ->
      case buildResultPage runTime raceDistance of
        Left err   -> return (Html.indexMaybeError (appErrorText err))
        Right page -> return page

-- | Validate the query parameters from the request
validateParams :: Maybe TimeParam -> Maybe DistanceParam -> Maybe CustomDistanceParam -> Either AppError (RT.RunTime, RaceDistance)
validateParams (Just (TimeParam timeText)) (Just (DistanceParam distChoice)) maybeCustomDist =
  case P.parseTime timeText of
    Left err -> Left (InputParseError err)
    Right runTime -> case P.resolveDistanceSelection
        distChoice (unwrapCustomDist <$> maybeCustomDist) of
          Left err           -> Left (InputParseError err)
          Right raceDistance -> Right (runTime, raceDistance)
validateParams _ _ _ = Left MissingRequiredInput

-- | Extract raw custom distance text from its wrapper
unwrapCustomDist :: CustomDistanceParam -> Text
unwrapCustomDist (CustomDistanceParam customDistText) = customDistText

-- | Build the result page for a valid time and distance combination
buildResultPage :: RT.RunTime -> RD.RaceDistance -> Either AppError (Html ())
buildResultPage runTime raceDistance =
  case raceTableOrError of
    Left err        -> Left (VdotCalculationError err)
    Right raceTable -> Right (Html.resultPage vdot raceTable intervalPaces)
  where
    totalSeconds = fromIntegral (RT.runTimeToSec runTime)
    vdot = calculateVDOT totalSeconds raceDistance
    raceTableOrError =
      sequence [ case equivalentTime vdot (presetDistance preset) of
          Left err   -> Left err
          Right time -> Right (presetLabel preset, RT.secToRunTime time)
        | preset <- presetRaceDistances ]

    intervalPaces = calculatePaces vdot

-- | Servant application entry point
app :: Application
app = serve api server

-- | Start the web application on port 6767
startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:6767"
  run 6767 app
