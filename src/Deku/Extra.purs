module Deku.Extra
  ( className
  ) where

import Prelude
import FRP.Event (Event)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Apply (lift2)
import Deku.DOM as H
import Deku.Attribute (Attribute, class Attr, (!:=))
import Deku.Attributes (klass)


className :: forall e. Attr e H.Class String => Array (String /\ Event Boolean) -> Event (Attribute e)
className = klass <<< classNameStr


classNameStr :: Array (String /\ Event Boolean) -> Event String 
classNameStr = Array.foldl 
  (\acc (label /\ event) -> 
    let f b s = if b then label <> " " <> s else s
    in lift2 f event acc
  )
  (pure "")
