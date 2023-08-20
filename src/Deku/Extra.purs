module Deku.Extra
  ( class Keys, keys
  , class KeysList, keysList
  ) where

import Prelude
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.RowList
import Type.Proxy

-- TODO: transform a Record<string, event> into a Event<string> class

class Keys row where
  keys :: Record row -> String

instance (RowToList row list, KeysList list) => Keys row where
  keys _ = keysList (Proxy :: _ list)

-- | Toy example that concatenates field names
-- class Keys :: (r :: Row Type) -> Constraint
class KeysList (r :: RowList Type) where
  keysList :: Proxy r -> String

instance KeysList Nil where
  keysList _ = ""

instance (KeysList tail, IsSymbol sym) => KeysList (Cons sym head tail) where
  keysList _ = reflectSymbol (Proxy :: _ sym) <> keysList (Proxy :: _ tail)
