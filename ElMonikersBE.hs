{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Servant
import Control.Monad.Trans.Either
import Network.Wai
import Network.Wai.Handler.Warp
import ElMonikersBE.Data (Card, Filter, cards, filters)

--          http://server/cards/de?limit=20
--          with a list of _Filter_s in an 'application/json' RequestBody [[{"f1":true,"f2":"Musician","f3":"Pop"},{"f1":false,"f2":"SciFi","f3":"Monster"}]
--       -> Serve up to 20 real pop musicians or fictional scifi monsters from the 'de' set of cards as JSON
type API = "cards"   :> Capture "locale" String :> QueryParam "limit" Int :> ReqBody '[JSON] [Filter] :> Post '[JSON] [Card]
--          http://server/filters/en
--       -> Serve a list of all available filters in the 'en' set of cards as JSON
      :<|> "filters" :> Capture "locale" String :> Get '[JSON] [Filter]

server :: Server API
server = getCards :<|> getFilters
   where getCards :: String -> Maybe Int -> [Filter] -> EitherT ServantErr IO [Card]
         getCards locale limit filter = return $ cards locale limit filter

         getFilters :: String -> EitherT ServantErr IO [Filter]
         getFilters l = return $ filters l

main :: IO ()
main = run 8081 $ serve myAPI server
    where myAPI :: Proxy API
          myAPI = Proxy
