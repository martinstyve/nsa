{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.List.NonEmpty as NE (toList)
import           Data.Set as Set          (toList)
import           Data.Text                (strip, Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char     (char, digitChar)

import           RunTime                  (RunTime(..))
import           RaceDistance             (RaceDistance(..))

data InputError
  = InvalidSeconds
  | InvalidMinutes
  | InvalidFormat
  | InvalidDistance
  | InvalidCustomDistance
  | MissingCustomDistance
  deriving (Show, Eq, Ord)

type Parser = Parsec InputError Text

digits :: Parser Int
digits = read <$> some digitChar

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

customDistanceParser :: Parser Int
customDistanceParser = do
  meters <- digits
  if 0 < meters && meters <= 50000 -- max distance 50km
    then pure meters
    else customFailure InvalidCustomDistance

parseTime :: Text -> Either InputError RunTime
parseTime = either (Left . bundleToInputError InvalidFormat) Right . runParser timeParser "" . strip

parseDistance :: Text -> Either InputError RaceDistance
parseDistance = either (Left . bundleToInputError InvalidDistance) Right . runParser distanceParser ""

parseCustomDistance :: Text -> Either InputError Int
parseCustomDistance = either (Left . bundleToInputError InvalidCustomDistance) Right . runParser customDistanceParser ""

resolveDistanceSelection :: Text -> Maybe Text -> Either InputError RaceDistance
resolveDistanceSelection selected maybeCustomDist =
  case strip selected of
    "custom" -> do
      meters <- case maybeCustomDist of
          Nothing             -> Left MissingCustomDistance
          Just customDist -> parseCustomDistance customDist
      pure $ CustomDistance (fromIntegral meters)
    distanceText -> parseDistance distanceText

-- https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec-Error.html#g:1
bundleToInputError :: InputError -> ParseErrorBundle Text InputError -> InputError
bundleToInputError fallback bundle =
  case [ err
       | FancyError _ ms <- NE.toList (bundleErrors bundle)
       , ErrorCustom err <- Set.toList ms
       ] of
    (e:_) -> e
    []    -> fallback

inputErrorText :: InputError -> Text
inputErrorText InvalidSeconds = "expected seconds 00-59"
inputErrorText InvalidMinutes = "expected minutes 00-59"
inputErrorText InvalidFormat = "use format h:mm:ss or mm:ss"
inputErrorText InvalidDistance = "choose a race distance"
inputErrorText InvalidCustomDistance = "enter a custom distance (meters)"
inputErrorText MissingCustomDistance = "enter a custom distance when custom is selected"
