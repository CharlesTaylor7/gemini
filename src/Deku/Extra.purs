module Deku.Extra
  ( class Class, className
  , class ClassRowList, className_
  ) where

import Prelude
import Prim.Row as Row
import Type.RowList (class RowToList, class ListToRow, RowList)
import Data.Symbol (class IsSymbol, reflectSymbol)
import FRP.Event (Event)
import Type.RowList (Cons, Nil)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- TODO: transform a Record<string, event> into a Event<string> class

class Class row where
  --className :: Record row -> Event String
  className :: Record row -> String

instance (RowToList row list, ListToRow list row, ClassRowList list) => Class row where
  className record = className_ record

-- | Toy example that concatenates field names
-- class Keys :: (r :: Row Type) -> Constraint
class ClassRowList (list :: RowList Type) where
  className_ :: forall row. ListToRow list row => Record row -> String

instance ClassRowList Nil where
  className_ _  = ""

instance (ClassRowList tail, IsSymbol sym) => ClassRowList (Cons sym head tail) where
  className_ record = reflectSymbol (Proxy :: _ sym) <> " " <> className_ (narrow record)
  -- (Proxy :: _ tail)

narrow :: forall row tail. Row.Cons _ _ tail row => Record row -> Record tail
narrow = unsafeCoerce
