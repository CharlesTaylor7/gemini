module Data.Nat
  ( module Data.Typelevel.Num.Sets
  , module Num
  , module Type.Proxy
  , knownInt
  , natsUnder
  ) where

import Prelude

import Data.Enum (enumFromTo)

import Data.Typelevel.Num.Aliases as Num
import Data.Typelevel.Num.Sets (class Nat, class Pos, toInt')

import Type.Proxy (Proxy(..))


knownInt :: forall n. Nat n => Proxy n -> Int
knownInt = toInt'

natsUnder :: forall bound. Nat bound => Proxy n -> Array Int
natsUnder proxy = enumFromTo 0 $ n - 1
  where n = knownInt proxy
