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
      input_ [type_ "number", name_ "time"]--, value_ "1080"]
      label_ "Distance"
      select_ [name_ "dist"] do
        option_ [value_ "5k"] "5km"
        option_ [value_ "10k"] "10km"
        option_ [value_ "marathon"] "marathon"
      button_ [type_ "submit"] "find vdot"




resultPage :: VDOT -> (Integer, Integer, Integer) -> Html ()
resultPage vdot (h, m, s) = do
  html_ do
      head_ do
          title_ "calc (slang for calculator)"
      body_ do
          h1_ "result"
          p_ $ "vdot: " >> toHtml (show vdot)
          p_ $ "hm time: " >> toHtml (show h ++ ":" ++ show m ++ ":" ++ show s)
          a_ [href_ "/"] "go back"

someText :: Text
someText = "good luck"

