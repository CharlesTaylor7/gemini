module Data.Nat
  ( module Data.Typelevel.Num.Sets
  , module Num
  , knownInt
  , natsUnder
  ) where

import Prelude
import Data.Enum (enumFromTo)
import Data.Typelevel.Num.Aliases as Num
import Data.Typelevel.Num (D1, D2, D3, D4, D5, D6, D7, D8, D9) as Num
import Data.Typelevel.Num.Sets (class Nat, class Pos, toInt')
import Type.Proxy (Proxy(..))


knownInt :: forall @n. Nat n => Int
knownInt = toInt' (Proxy :: _ n)

natsUnder :: forall @bound. Nat bound => Array Int
natsUnder = enumFromTo 0 $ n - 1
  where
  n = knownInt @bound
