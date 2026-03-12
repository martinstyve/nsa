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
        option_ [value_ "marathon"] "marathon"
      button_ [type_ "submit"] "find vdot"



-- todo ?
-- RaceTime :: Text
-- or InputTime :: Text
resultPage :: VDOT -> Text -> Html ()
resultPage vdot timeText = do
  html_ do
      head_ do
          title_ "calc (slang for calculator)"
      body_ do
          h1_ "result"
          p_ $ "vdot: " >> toHtml (show vdot)
          p_ $ "hm time: " >> toHtml timeText
          a_ [href_ "/"] "go back"

someText :: Text
someText = "good luck"

