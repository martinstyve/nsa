{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import RunTime
import VDOT

import Data.List.NonEmpty as NE
import Data.Set as Set

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

