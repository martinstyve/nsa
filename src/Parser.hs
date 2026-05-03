{-# LANGUAGE OverloadedStrings #-}

{-| Module      : Parser
Description : Parse and validate calculator form input

Validates race times, distances, and custom distance values, and turns
parser failures into UI error messages
-}
module Parser where

import           Data.List.NonEmpty as NE (toList)
import           Data.Set as Set          (toList)
import           Data.Text                (strip, Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char     (char, digitChar)

import           RunTime                  (RunTime(..))
import           RaceDistance             (RaceDistance(..))

-- | Parser errors for calculator inputs
--
-- Constructors:
--
-- * @InvalidSeconds@ - seconds outside the 00-59 range
-- * @InvalidMinutes@ - minutes outside the 00-59 range in h:mm:ss input
-- * @InvalidFormat@ - input does not match a supported time format
-- * @InvalidDistance@ - selected race distance is not one of the presets
-- * @InvalidCustomDistance@ - custom distance is missing or out of range
-- * @MissingCustomDistance@ - custom distance was not provided
data InputError
  = InvalidSeconds
  | InvalidMinutes
  | InvalidFormat
  | InvalidDistance
  | InvalidCustomDistance
  | MissingCustomDistance
  deriving (Show, Eq, Ord)

-- | Megaparsec parser used for calculator inputs
type Parser = Parsec InputError Text

-- | Parse one or more decimal digits into an integer
digits :: Parser Int
digits = read <$> some digitChar

-- | Parse a time value in mm:ss or h:mm:ss format
--
-- Accepts either @mm:ss@ or @h:mm:ss@ and rejects values with too many
-- parts or minutes and seconds outside the 00-59 range
timeParser :: Parser RunTime
timeParser = do
  parts <- digits `sepBy1` char ':'
  case parts of
    [m, s] ->
      if s < 60
        then pure (MS m s)
        else customFailure InvalidSeconds
    [h, m, s] ->
      if m >= 60
        then customFailure InvalidMinutes
        else if s >= 60
               then customFailure InvalidSeconds
               else pure (HMS h m s)
    _ -> customFailure InvalidFormat

-- | Parse a preset race distance label
--
-- Accepts the fixed distance labels used by the UI select field
distanceParser :: Parser RaceDistance
distanceParser = choice
    [ FifteenHundred <$ "1500m"
    , OneMile <$ "mile"
    , ThreeK <$ "3000m"
    , FiveK <$ "5k"
    , TenK <$ "10k"
    , TenMile <$ "10mile"
    , HalfMarathon <$ "half"
    , Marathon <$ "marathon" ]

-- | Parse a custom distance in meters
--
-- Accepts a positive integer up to 50000 meters
customDistanceParser :: Parser Int
customDistanceParser = do
  meters <- digits
  if 0 < meters && meters <= 50000 -- max distance 50km
    then pure meters
    else customFailure InvalidCustomDistance

-- | Parse a race time in mm:ss or h:mm:ss format
parseTime :: Text -> Either InputError RunTime
parseTime = either (Left . bundleToInputError InvalidFormat) Right . runParser timeParser "" . strip

-- | Parse one of the preset race distance labels
parseDistance :: Text -> Either InputError RaceDistance
parseDistance = either (Left . bundleToInputError InvalidDistance) Right . runParser distanceParser ""

-- | Parse a custom race distance in meters
parseCustomDistance :: Text -> Either InputError Int
parseCustomDistance = either (Left . bundleToInputError InvalidCustomDistance) Right . runParser customDistanceParser "" . strip

-- | Resolve the selected distance option, including a custom distance
resolveDistanceSelection :: Text -> Maybe Text -> Either InputError RaceDistance
resolveDistanceSelection selected maybeCustomDist =
  case strip selected of
    "custom" -> do
      meters <- case maybeCustomDist of
          Nothing             -> Left MissingCustomDistance
          Just customDist -> parseCustomDistance customDist
      pure $ CustomDistance (fromIntegral meters)
    distanceText -> parseDistance distanceText

-- | Extract the custom parser error from a Megaparsec bundle
--
-- Read more [here](https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec-Error.html#g:1)
bundleToInputError :: InputError -> ParseErrorBundle Text InputError -> InputError
bundleToInputError fallback bundle =
  case [ err
       | FancyError _ ms <- NE.toList (bundleErrors bundle)
       , ErrorCustom err <- Set.toList ms
       ] of
    (e:_) -> e
    []    -> fallback

-- | Text form for each parser error
inputErrorText :: InputError -> Text
inputErrorText InvalidSeconds = "expected seconds 00-59"
inputErrorText InvalidMinutes = "expected minutes 00-59"
inputErrorText InvalidFormat = "use format h:mm:ss or mm:ss"
inputErrorText InvalidDistance = "choose a race distance"
inputErrorText InvalidCustomDistance = "enter a custom distance (meters)"
inputErrorText MissingCustomDistance = "enter a custom distance when custom is selected"
