module Data.Nat
  ( class Nat
  , class Pos
  , class Lt
  , knownInt
  , natsUnder
  ) where

import Prelude

import Data.Enum (enumFromTo)
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))

class Nat :: Int -> Constraint
class Reflectable n Int <= Nat n

class Pos :: Int -> Constraint
class (Compare n 0 GT, Nat n) <= Pos n

class (Compare a b LT) <= Lt a b

instance Reflectable n Int => Nat n
instance (Compare n 0 GT, Nat n) => Pos n
instance (Compare a b LT) => Lt a b

knownInt :: forall @n. Nat n => Int
knownInt = reflectType (Proxy :: _ n)

natsUnder :: forall @bound. Nat bound => Array Int
natsUnder = enumFromTo 0 $ n - 1
  where
  n = knownInt @bound
