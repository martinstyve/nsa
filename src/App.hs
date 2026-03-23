{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Lucid                    (Html)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid

import           Data.Text                (Text)


import           Html
import           PaceRange -- todo
import           RunTime as RT
import           VDOT
import           Parser as P
import Data.Maybe (fromMaybe)

-- servant docs tutorial and source code
-- https://docs.servant.dev/en/latest/tutorial/
-- https://github.com/haskell-servant/
-- https://docs.servant.dev/en/latest/tutorial/ApiType.html 06.03.26

-- todo params must be RunTime and RaceDistance types
type API = 
    Get '[HTML] (Html ()) 
    :<|> "result" :> QueryParam "time" Text :> QueryParam "dist" Text :> QueryParam "custom_dist" Text :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server = homeHandler :<|> calcHandler
  where
    homeHandler = return Html.index

    calcHandler (Just t) (Just d) customD = do
      let distToParse = if d == "custom" then fromMaybe "" customD else d
      case (P.parseTime t, P.parseDistance distToParse) of
        (Left err, _) -> return $ Html.indexMaybeError (Just (P.inputErrorText err))
        (_, Left err) -> return $ Html.indexMaybeError (Just (P.inputErrorText err))
        (Right runTime, Right dist) -> do
          let totalSeconds = fromIntegral $ RT.runTimeToSec runTime
          let vdot = calculateVDOT totalSeconds dist
          let raceTable = [ ("5k", RT.formatRunTime (equivalentTime vdot FiveK))
                          , ("10k", RT.formatRunTime (equivalentTime vdot TenK))
                          , ("half", RT.formatRunTime (equivalentTime vdot HalfMarathon))
                          , ("marathon", RT.formatRunTime (equivalentTime vdot Marathon))
                          ]
          let intervalPaces = calculatePaces vdot
          return $ Html.resultPage vdot raceTable intervalPaces

    calcHandler _ _ _ = return Html.index

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  putStrLn "visit http://localhost:3000"
  run 3000 app
