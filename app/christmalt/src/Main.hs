{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import Database.HDBC (commit, disconnect, execute, fetchRow, prepare, toSql)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import qualified Data.Text as T

connectionString =
  "host=localhost dbname=beers user=postgres password=postgres"

foo :: IO ()
foo = do
  c <- connectPostgreSQL connectionString
  random <- prepare c randomBeer
  details <- prepare c beerDetails
  execute random []
  beer <- fetchRow random
  result <- case beer of
    Just id -> do
      consume <- prepare c consumeBeer
      execute consume id
      execute details id
      fetchRow details
    _ -> return Nothing
  print result
  commit c
  disconnect c
  return ()

randomBeer =
  unlines [
    "select beers.id"
  , "from beers"
  , "left join history"
  , "on beers.id = history.beer"
  , "where history.consumed is null"
  , "order by random()"
  , "limit 1"
  ]

beerDetails =
  unlines [
    "select beers.name, breweries.name, breweries.location, styles.name, beers.abv"
  , "from beers"
  , "join breweries"
  , "on beers.brewery = breweries.id"
  , "join styles"
  , "on beers.style = styles.id"
  , "where beers.id = ?"
  ]

consumeBeer =
  "insert into history(beer, consumed) values(?, current_date)"

-- >>>>>>>>>>

data Message = Message {
  messageTo :: T.Text
, messageFrom :: T.Text
, messageBody :: T.Text
} deriving Show

instance ToFormUrlEncoded Message where
  toFormUrlEncoded m = [("To", messageTo m), ("From", messageFrom m), ("Body", messageBody m)]

type Messages =
     "2010-04-01"
  :> "Accounts"
  :> Capture "AccountSid" String
  :> ReqBody '[FormUrlEncoded] Message
  :> Post '[JSON] ()

api :: Proxy Messages
api = Proxy

messages :: String -> Message -> Manager -> BaseUrl -> ExceptT ServantError IO ()
messages = client api

baseUrl = BaseUrl Https "api.twilio.com" 443 ""

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  print result

