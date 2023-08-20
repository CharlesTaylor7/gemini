module Data.Nat
  ( class KnownNat
  , knownInt
  , natsUnder
  ) where

import Prelude

import Data.Enum (enumFromTo)
import Data.Typelevel.Num.Sets (class Nat, toInt')

import Type.Proxy (Proxy(..))


class Nat a <= KnownNat a
instance Nat a => KnownNat a

knownInt :: forall n. Nat n => Int
knownInt = toInt' (Proxy :: _ n)

natsUnder :: forall bound. Nat bound => Array Int
natsUnder = enumFromTo 0 $ n - 1
  where n = toInt' (Proxy :: _ bound)
