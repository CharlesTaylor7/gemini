{-# language PackageImports #-}
module Rel8
  ( module Rel8_ 
  , Id(..), nextId
  , AutoSchema(..), each
  , Day
  )
  where

-- relude
import Relude

-- base
import GHC.Records (HasField)
import Data.Typeable (typeRep)

-- rel8
import "rel8" Rel8 as Rel8_ hiding (each)
import qualified "rel8" Rel8 as R

-- postgresql-binary
import PostgreSQL.Binary.Data (Day)

-- aeson-casing
import Data.Aeson.Casing (snakeCase)

-- http-api-data
import Web.HttpApiData (FromHttpApiData)


-- | Type safe ids
newtype Id m = Id Int64
  deriving stock Show
  deriving newtype (DBType, DBEq, DBOrd, FromHttpApiData)


-- | Make schemas auto derivable
class (Table Name (m Name), Typeable m) => AutoSchema m where
  autoSchema :: TableSchema (m Name)
  autoSchema = TableSchema
    { name = snakeCase . show . typeRep $ (Proxy :: Proxy m)
    , schema = Nothing
    , columns = namesFromLabelsWith $ snakeCase . concat
    }


-- | Generate an expresion for the auto id column of table
-- | Note: this assumes that your primary key is named "id" and is a serial8
nextId
  :: forall m.
    ( AutoSchema m
    , HasField "id" (m Expr) (Column Expr (Id m))
    )
  => Expr (Id m)
nextId =
  -- this cast is safe because Id has the exact same instance of DBType as Int64
  coerce $
  R.nextval $
  name (autoSchema @m) <> "_id_seq"


each
  :: forall m names exprs.
    ( Selects names exprs
    , AutoSchema m
    , m Name ~ names
    )
  => Query exprs
each = R.each $ autoSchema @m
