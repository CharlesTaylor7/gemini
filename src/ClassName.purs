module ClassName 
  ( name
  , when
  )
  where

import Prelude
import Effect (Effect)
import FRP.Event (Event)
import Data.Foldable (foldMap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Apply (lift2)
import Deku.DOM as D
import Deku.Attribute (Attribute, class Attr, cb, Cb, unsafeAttribute, AttributeValue(..))
import Deku.Attributes (klass)
import Web.Event.Event as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Unsafe.Coerce (unsafeCoerce)
import Safe.Coerce (coerce)


newtype ClassName
  = ClassName String

type ClassEntry = Event String 


when :: String -> Event Boolean -> Event String
when name = map (if _ then name else "")


name :: forall e. Attr e D.Class String => Array (Event String) -> Event (Attribute e)
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
