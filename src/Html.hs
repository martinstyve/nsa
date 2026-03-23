{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Html where

import           Data.Text (Text)
import           Lucid
import           PaceRange
import qualified RunTime as RT
import           VDOT

index :: Html ()
index = indexMaybeError Nothing


-- todo only display custom distance when custom is selected
-- radio buttons and show when checked?
indexMaybeError :: Maybe Text -> Html ()
indexMaybeError maybeError = do
  html_ do
    head_ do
      title_ "hello haskell"
      style_ "body { background-color: green; }"
    body_ do
      h1_ "My calc"
      maybe mempty (p_ . toHtml) maybeError
      form_ [action_ "/result", method_ "get"] do
        p_ do
          label_ "Time"
          input_ [type_ "text", name_ "time", placeholder_ "18:30"]
        p_ do
          label_ "Distance"
          select_ [name_ "dist"] do
            option_ [value_ "5k"] "5km"
            option_ [value_ "10k"] "10km"
            option_ [value_ "half"] "Half Marathon"
            option_ [value_ "marathon"] "Marathon"
            option_ [value_ "custom"] "Custom Distance"
        input_ [type_ "text", name_ "custom", placeholder_ "e.g. 8.5"]
        button_ [type_ "submit"] "find vdot"



-- todo ?
-- RaceTime :: Text
-- or InputTime :: Text
resultPage :: VDOT -> [(Text, Text)] -> [PaceRange] -> Html ()
resultPage vdot raceTable intervalPaces = do
  html_ do
    head_ $ title_ "calc (slang for calculator)"
    body_ do
      h1_ "result"
      p_ $ "VDOT: " >> toHtml (show vdot)
      h2_ "equiv race times"
      table_ do
        mapM_ (\(d, t) -> tr_ (td_ (toHtml d) >> td_ (toHtml t))) raceTable
      h2_ "interval pace ranges"
      table_ do
        tr_ do
          th_ "Zone"
          th_ "Pace range"
          th_ "Intensity"
        mapM_ trainingSection intervalPaces
      a_ [href_ "/"] "go back"

trainingSection :: PaceRange -> Html ()
trainingSection pace = tr_ do
  td_ (toHtml (name pace))
  td_ (toHtml paceText)
  td_ (toHtml (intensity pace))
  where
    minText = RT.formatRunTime (minPace pace)
    maxText = RT.formatRunTime (maxPace pace)
    paceText = minText <> " - " <> maxText <> " /km"
