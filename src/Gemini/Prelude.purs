module Gemini.Prelude
  ( module Prelude
  , module Data.Nat
  , module Data.Angle
  , module Data.Cyclic
  , module Data.Finitary 
  , module Data.Group 
  , module Data.Set
  , module Data.Map
  , module Data.Maybe
  , module Data.Foldable
  , module Data.Tuple.Nested
  , module Effect
  , module Effect.Console
  ) where

import Prelude
import Data.Nat
import Data.Angle
import Data.Cyclic
import Data.Finitary 
import Data.Group
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Foldable (fold)
import Data.Tuple.Nested (type (/\), (/\))

import Effect (Effect)
import Effect.Console (log)
