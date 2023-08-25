module Gemini.Prelude
  ( module Prelude
  , module Debug
  , module Data.Nat
  , module Data.Angle
  , module Data.Cyclic
  , module Data.Finitary 
  , module Data.Group 
  , module Data.Gemini
  , module Data.Set
  , module Data.Map
  , module Data.Maybe
  , module Data.Foldable
  , module Data.Tuple.Nested
  , module Effect
  , module Effect.Console
  , module Partial.Unsafe
  , module Deku.Attribute
  , module FRP.Event
  , module Gemini.Types
  ) where

import Prelude hiding (class Ring)
import Debug (spy)
import Data.Nat
import Data.Angle
import Data.Cyclic
import Data.Finitary 
import Data.Group
import Data.Gemini 
  ( Gemini
  , Rotation(..)
  , rotation
  , RotationDirection(..)
  , Motion(..)
  , Ring(..)
  , Location(..)
  )
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Foldable (fold)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)

import Deku.Attribute (Attribute)
import FRP.Event (Event)

import Gemini.Types (AppState)
