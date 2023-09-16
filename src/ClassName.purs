module ClassName
  ( name
  , when
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (foldMap)
import Deku.Attribute (class Attr, Attribute, AttributeValue(..), Cb, cb, unsafeAttribute)
import Deku.Attributes (klass)
import Deku.DOM as D
import FRP.Event (Event)
import Safe.Coerce (coerce)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

newtype ClassName = ClassName String

type ClassEntry = Event String

when :: Event Boolean -> String -> Event String
when event name = event <#> if _ then name else ""

name ::
  forall e. Attr e D.Class String => Array (Event String) -> Event (Attribute e)
name = klass <<< coerce <<< foldMap coerceEvent
  where
  coerceEvent :: Event String -> Event ClassName
  coerceEvent = coerce

instance Semigroup ClassName where
  append a@(ClassName x) b@(ClassName y)
    | x == "" = b
    | y == "" = a
    | otherwise =
        ClassName
          $ x
              <> " "
              <> y

instance Monoid ClassName where
  mempty = ClassName ""
