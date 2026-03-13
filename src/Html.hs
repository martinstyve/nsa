{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Html where

import           Data.Text (Text)
import           Lucid
import           PaceRange
import           VDOT

index :: Html ()
index = do
  html_ do
    head_ do
      title_ "hello haskell"
      style_ "body { background-color: green; }"
    body_ do
      h1_ "My calc"
      form_ [action_ "/result", method_ "get"] do
      label_ "Time"
      input_ [type_ "text", name_ "time", placeholder_ "18:30"]
      label_ "Distance"
      select_ [name_ "dist"] do
        option_ [value_ "5k"] "5km"
        option_ [value_ "10k"] "10km"
        -- HM missing
        option_ [value_ "marathon"] "marathon"
      button_ [type_ "submit"] "find vdot"



-- todo ?
-- RaceTime :: Text
-- or InputTime :: Text
resultPage :: VDOT -> [(Text, Text)] -> Html ()
resultPage vdot raceTable = do
  html_ do
    head_ $ title_ "calc (slang for calculator)"
    body_ do
      h1_ "result"
      p_ $ "VDOT: " >> toHtml (show vdot)
      h2_ "equiv race times"
      table_ do
        mapM_ (\(d, t) -> tr_ (td_ (toHtml d) >> td_ (toHtml t))) raceTable
      a_ [href_ "/"] "go back"
