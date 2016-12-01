module Main where

import Control.Monad.Aff ( Canceler, launchAff )
import Control.Monad.Eff ( Eff )
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Eff.Console ( CONSOLE, log )
import Control.Monad.Eff.Exception ( EXCEPTION )
import Data.Either ( Either (..) )
import Data.FormURLEncoded ( FormURLEncoded (..), encode )
import Data.Functor ( void )
import Data.HTTP.Method ( Method (..) )
import Data.Maybe ( Maybe (.. ), maybe )
import Data.MediaType.Common ( applicationFormURLEncoded )
import Data.String ( Pattern (..), charAt, contains, singleton )
import Data.Tuple ( Tuple (..) )
import Network.HTTP.Affjax ( AJAX, Affjax, AffjaxRequest, affjax, defaultRequest )
import Network.HTTP.RequestHeader ( RequestHeader (..) )
import Prelude
import Text.Base64 ( encode64 )

import BeerDatabase

main = launchAff $ do
  beer <- getBeer
  case beer of
    Just b -> void ( ( affjax ( request ( formatBeer b ) ) ) :: forall e. Affjax e Unit )
    Nothing -> pure unit
  liftEff $ log ( maybe "No beer available" formatBeer beer )

request :: String -> AffjaxRequest String
request s = defaultRequest {
         method = Left POST
       , url = messagesPath
       , headers = [ createAuthHeader authUser authPass, formType ]
       , content = Just ( encode ( formatMessage ( Message { to: "+61418481214", from: "+61481072793", body: s } ) ) )
       , username = Just authUser
       , password = Just authPass
       , withCredentials = true
       }

formatBeer :: BeerDetails -> String
formatBeer ( BeerDetails d ) = 
     "Today you'll be drinking the " <> d.name <> " by " <> d.brewery <> " ("
     <> d.location <> "). It's " <> ( definite d.style ) <> " with an ABV of "
     <> d.abv <> "%. Cheers!"

definite :: String -> String
definite n = case charAt 0 n of
  Just l -> if contains ( Pattern ( singleton l ) ) "AEIOUaeiou"
               then "an " <> n
               else "a " <> n
  Nothing -> ""

newtype Message = Message {
  to :: String,
  from :: String,
  body :: String
}

formatMessage :: Message -> FormURLEncoded
formatMessage ( Message { to, from, body } ) =
  FormURLEncoded [ Tuple "To" ( Just to )
                 , Tuple "From" ( Just from )
                 , Tuple "Body" ( Just body ) ]

messagesPath :: String
messagesPath = "https://api.twilio.com/2010-04-01/Accounts/AC4eb099db729aa4758f0415a0a5231aea/Messages"

authUser :: String
authUser = "N/A"

authPass :: String
authPass = "N/A"

createAuthHeader :: String -> String -> RequestHeader
createAuthHeader user pass =
  let
    headerName = "Authorization"
    headerValue = "Basic " <> encode64 ( user <> ":" <> pass )
  in
    RequestHeader headerName headerValue

formType :: RequestHeader
formType = ContentType applicationFormURLEncoded 
