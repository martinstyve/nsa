{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative

data RunTime
  = MS Int Int -- mm:ss
  | HMS Int Int Int -- h:mm:ss
  deriving (Show, Eq)

type Parser = Parsec Void Text

twoDigits :: Parser Int
twoDigits = do
  first <- digitChar
  second <- digitChar
  return $ read [first, second]

digits :: Parser Int
digits = L.decimal

hmsParser :: Parser RunTime
hmsParser = do
  hours <- digits
  _ <- char ':'
  minutes <- twoDigits
  _ <- char ':'
  seconds <- twoDigits
  if minutes >= 60
    then fail "minutes over 60 sec"
    else if seconds >= 60
      then fail "seconds over 60 sec"
      else return $ HMS hours minutes seconds

msParser :: Parser RunTime
msParser = do
  minutes <- digits
  _ <- char ':'
  seconds <- twoDigits
  if seconds >= 60 then fail "over 60 sec" else return $ MS minutes seconds

timeParser :: Parser RunTime
timeParser = try hmsParser <|> msParser

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

