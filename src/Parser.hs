{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import VDOT

import Data.List.NonEmpty as NE
import Data.Set as Set

data RunTime
  = MS Int Int -- mm:ss
  | HMS Int Int Int -- h:mm:ss
  deriving (Show, Eq)

data InputError 
  = InvalidSeconds 
  | InvalidMinutes
  | InvalidDistance
  | InvalidFormat 
  deriving (Show, Eq, Ord)

type Parser = Parsec InputError Text

digits :: Parser Int
digits = read <$> some digitChar

timeParser :: Parser RunTime
timeParser = do
  parts <- digits `sepBy1` char ':'
  case parts of
    [m, s]    | s < 60 -> pure (MS m s)
    [h, m, s] | m < 60 && s < 60 -> pure (HMS h m s)
    _         -> customFailure InvalidFormat

distanceParser :: Parser RaceDistance
distanceParser = choice
  [ FiveK <$ "5k"
  , TenK <$ "10k"
  , HalfMarathon <$ "half"
  , Marathon <$ "marathon"
  ] <|> customFailure InvalidDistance

parseTime :: Text -> Either InputError RunTime
parseTime = either (Left . bundleToInputError) Right . runParser timeParser ""

parseDistance :: Text -> Either InputError RaceDistance
parseDistance = either (Left . bundleToInputError) Right . runParser distanceParser ""

bundleToInputError :: ParseErrorBundle Text InputError -> InputError
bundleToInputError bundle =
  case [err | FancyError _ ms <- NE.toList (bundleErrors bundle)
            , ErrorCustom err <- Set.toList ms ] of
    (e:_) -> e
    [] -> InvalidFormat

inputErrorText :: InputError -> Text
inputErrorText InvalidSeconds = "expected seconds 00-59"
inputErrorText InvalidMinutes = "expected minutes 00-59"
inputErrorText InvalidDistance = "choose distance from dropdown"
inputErrorText InvalidFormat = "use format h:mm:ss or mm:ss"

-- move this and keep Parser solely from text parsing
runTimeToSec :: RunTime -> Integer
runTimeToSec (MS m s) = fromIntegral (m * 60 + s)
runTimeToSec (HMS h m s) = fromIntegral (h * 3600 + m * 60 + s)

formatRunTime :: Integer -> Text
formatRunTime totalSeconds = pack $
  if hours > 0 
    then show hours ++ ":" ++ zeroPad mins ++ ":" ++ zeroPad secs
    else zeroPad mins ++ ":" ++ zeroPad secs
  where
    hours = totalSeconds `div` 3600
    mins  = (totalSeconds `mod` 3600) `div` 60
    secs  = totalSeconds `mod` 60
    zeroPad n = if n < 10 then "0" ++ show n else show n

