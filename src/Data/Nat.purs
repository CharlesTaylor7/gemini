module Data.Nat
  ( class Nat
  , class Pos
  , knownInt
  , natsUnder
  , module Prim.Int
  , module Prim.Ordering
  ) where

import Prelude

import Data.Enum (enumFromTo)
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class Compare)
import Prim.Ordering (EQ, GT, LT)
import Type.Proxy (Proxy(..))

class Nat :: Int -> Constraint
class (Compare n (-1) GT, Reflectable n Int) <= Nat n

class Pos :: Int -> Constraint
class (Compare n 0 GT, Nat n) <= Pos n

class (Compare a b LT) <= Lt a b

instance (Compare n (-1) GT, Reflectable n Int) => Nat n
instance (Compare n 0 GT, Nat n) => Pos n

knownInt :: forall @n. Nat n => Int
knownInt = reflectType (Proxy :: _ n)

natsUnder :: forall @bound. Nat bound => Array Int
natsUnder = enumFromTo 0 $ n - 1
  where
  n = knownInt @bound
