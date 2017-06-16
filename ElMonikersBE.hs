{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import ElMonikersBE.Data (Card, cards, cardExample, Filter, filters, filterExample)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Docs
import Servant.JS
import System.IO

--          http://server/cards/de?limit=20
--          with a list of _Filter_s in an 'application/json' RequestBody [{"f1":true,"f2":"Musician","f3":"Pop"},{"f1":false,"f2":"SciFi","f3":"Monster"}]
--       -> Serve up to 20 real pop musicians or fictional scifi monsters from the 'de' set of cards as JSON
type RestApi = "cards"   :> Capture "locale" String :> QueryParam "limit" Int :> ReqBody '[JSON] [Filter] :> Post '[JSON] [Card]
--          http://server/filters/en
--       -> Serve a list of all available filters in the 'en' set of cards as JSON
      :<|> "filters" :> Capture "locale" String :> Get '[JSON] [Filter]
--          http://server/api
--       -> serve jquery client functions as plain text
      :<|> "api" :> QueryFlag "useAngular" :> Get '[JSON, PlainText] Text
--          http://server/docs
--       -> serve documentation of api as markdown
      :<|> "docs" :> Get '[JSON] Text
--          http://server/status (use http://server/status?lie to get an error)
--       -> serve status of server
      :<|> "status" :> QueryFlag "lie" :> Get '[JSON] Text

server :: Server RestApi
server = getCards :<|> getFilters :<|> getApi :<|> getDocs :<|> getStatus
   where getCards :: String -> Maybe Int -> [Filter] -> Handler [Card]
         getCards locale limit fs = return $ cards locale limit fs

         getFilters :: String -> Handler [Filter]
         getFilters l = return $ filters l

         getApi :: Bool -> Handler Text
         getApi useAngular | useAngular = prepare $ angular defAngularOptions
                           | otherwise  = prepare jquery
                           where prepare = return . jsForAPI myRestApi

         getDocs :: Handler Text
         getDocs = (return . pack . markdown . docs) myRestApi

         getStatus :: Bool -> Handler Text
         getStatus False = return $ pack "I'm a leaf on the wind. Watch how I soar..."
         getStatus True  = throwError (err500 { errBody = "Oh god, oh god, we're all gonna die!" })

myRestApi :: Proxy RestApi
myRestApi = Proxy

myServer :: IO Application
myServer = return $ serve myRestApi server

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


instance ToCapture (Capture "locale" String) where
  toCapture _ = DocCapture "locale" "locale id (i.e. 'de', 'en')"

instance ToParam (QueryParam "limit" Int) where
  toParam _ = DocQueryParam "limit" ["1","42","2112"] "limit to query results; cut off after n results" Normal

instance ToParam (QueryFlag "lie") where
  toParam _ = DocQueryParam "lie" ["lie"] "server should lie about status" Flag

instance ToParam (QueryFlag "useAngular") where
  toParam _ = DocQueryParam "useAngular" ["useAngular"] "prepare client js in angular (instead of jquery)" Flag

instance ToSample Text where
  toSamples _ = singleSample $ pack "Api-Documentation in markdown"

instance ToSample Filter where
  toSamples _ = singleSample filterExample

instance ToSample Card where
  toSamples _ = singleSample cardExample
