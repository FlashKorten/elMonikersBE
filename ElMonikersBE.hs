{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Text as T (Text)
import ElMonikersBE.Data (Card, Filter, cards, filters)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.JS
import System.IO

--          http://server/cards/de?limit=20
--          with a list of _Filter_s in an 'application/json' RequestBody [{"f1":true,"f2":"Musician","f3":"Pop"},{"f1":false,"f2":"SciFi","f3":"Monster"}]
--       -> Serve up to 20 real pop musicians or fictional scifi monsters from the 'de' set of cards as JSON
type API = "cards"   :> Capture "locale" String :> QueryParam "limit" Int :> ReqBody '[JSON] [Filter] :> Post '[JSON] [Card]
--          http://server/filters/en
--       -> Serve a list of all available filters in the 'en' set of cards as JSON
      :<|> "filters" :> Capture "locale" String :> Get '[JSON] [Filter]
--          http://server/api
--       -> serve jquery client functions as plain text
      :<|> "api" :> Get '[JSON, PlainText] Text

server :: Server API
server = getCards :<|> getFilters :<|> getApi
   where getCards :: String -> Maybe Int -> [Filter] -> Handler [Card]
         getCards locale limit fs = return $ cards locale limit fs

         getFilters :: String -> Handler [Filter]
         getFilters l = return $ filters l

         getApi :: Handler Text
         getApi = return $ jsForAPI myAPI jquery

api2JS :: Text
api2JS = jsForAPI myAPI jquery

myAPI :: Proxy API
myAPI = Proxy

myServer :: IO Application
myServer = return $ serve myAPI server

main :: IO ()
main =
    withStdoutLogger $ \apLogger -> do
      let port = 3000
          settings =
            setPort port $
            setLogger apLogger $
            setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
      runSettings settings =<< myServer
