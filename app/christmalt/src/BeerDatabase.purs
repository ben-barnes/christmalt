module BeerDatabase where

import Control.Monad.Aff ( Aff )
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Maybe.Trans ( MaybeT (..), runMaybeT )
import Control.Monad.Trans.Class ( lift )
import Data.Either ( Either (..) )
import Data.Foreign.Class ( class IsForeign )
import Data.Foreign.Generic ( readGeneric, defaultOptions )
import Data.Generic ( class Generic, gShow )
import Data.Maybe ( Maybe (..), maybe )
import Database.Postgres
import Database.Postgres.SqlValue
import Database.Postgres.Transaction
import Prelude ( ($), (<<<), class Show, Unit, bind, const, id, pure )

newtype BeerDetails = BeerDetails {
  name :: String,
  brewery :: String,
  location :: String,
  style :: String,
  abv :: String
}

derive instance genericBeerDetails :: Generic BeerDetails

instance isForeignBeerDetails :: IsForeign BeerDetails where
  read = readGeneric defaultOptions { unwrapNewtypes = true }

instance showBeerDetails :: Show BeerDetails where
  show = gShow

connectionInfo :: ConnectionInfo
connectionInfo = {
  host: "localhost",
  db: "beers",
  port: 5432,
  user: "postgres",
  password: "postgres"
}

getBeer :: forall eff. Aff ( db :: DB | eff ) ( Maybe BeerDetails )
getBeer = withConnection connectionInfo (\c -> runMaybeT ( getTodaysBeer c ) )

getTodaysBeer :: forall eff. Client -> MaybeT ( Aff ( db :: DB | eff ) ) BeerDetails
getTodaysBeer c = MaybeT ( queryOne_ getTodaysBeerQuery c )

getTodaysBeerQuery :: Query BeerDetails
getTodaysBeerQuery = Query
  """
  select name, brewery, location, style, abv
  from details
  where consumeon = current_date
  """
