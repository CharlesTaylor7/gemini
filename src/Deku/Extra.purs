module Deku.Extra
  ( className
  ) where

import Prelude
import FRP.Event (Event)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Apply (lift2)


className :: Array (String /\ Event Boolean) -> Event String
className = Array.foldl 
  (\acc (label /\ event) -> 
    let f b s = if b then label <> " " <> s else s
    in lift2 f event acc
  )
  (pure "")
