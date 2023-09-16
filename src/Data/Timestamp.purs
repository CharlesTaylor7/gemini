module Data.Timestamp
  ( Timestamp(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)

-- | timestamp in milliseconds
newtype Timestamp = Timestamp { milliseconds :: Int }

derive instance Eq Timestamp
derive instance Generic Timestamp _
