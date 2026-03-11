{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Html where

import           Data.Text (Text, pack)
import           Lucid
import           PaceRange
import           VDOT

page :: VDOT -> (Integer, Integer, Integer) -> Html ()
page vdot (h, m, s) = do
  html_ do
      head_ do
          title_ "calc (slang for calculator)"
      body_ do
          p_ do
            "vdot: "
            toHtml (show vdot)
          p_ do
            p_ "hm time: "
            toHtml $ show h ++ ":" ++ show m ++ ":" ++ show s
          p_ do
            toHtml someText

someText :: Text
someText = "good luck"
