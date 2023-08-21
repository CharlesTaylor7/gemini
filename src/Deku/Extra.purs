module Deku.Extra
  ( class Class, className
  , class ClassRowList, className_
  ) where

import Prelude
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import FRP.Event (Event)
import Type.RowList
import Type.Proxy

-- TODO: transform a Record<string, event> into a Event<string> class

class Class row where
  --className :: Record row -> Event String
  className :: Record row -> String

instance (RowToList row list, ClassRowList list) => Class row where
  className record = className_ record

-- | Toy example that concatenates field names
-- class Keys :: (r :: Row Type) -> Constraint
class ClassRowList (list :: RowList Type) where
  className_ :: forall row. RowToList row list => Record row -> String

instance ClassRowList Nil where
  className_ _ = ""

instance (ClassRowList tail, IsSymbol sym) => ClassRowList (Cons sym head tail) where
  className_ _ = reflectSymbol (Proxy :: _ sym) <> " "<> className_ (Proxy :: _ tail)
