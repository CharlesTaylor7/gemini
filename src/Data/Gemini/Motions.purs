module Data.Gemini.Motions where

import Prelude
import Data.Gemini
import Data.Cyclic (cyclic)

l :: Int -> Motion
l amount = Motion { amount: cyclic amount, ring: LeftRing }

l' :: Int -> Motion
l' amount = Motion { amount: cyclic $ negate amount, ring: LeftRing }

c :: Int -> Motion
c amount = Motion { amount: cyclic amount, ring: CenterRing }

c' :: Int -> Motion
c' amount = Motion { amount: cyclic $ negate amount, ring: CenterRing }

r :: Int -> Motion
r amount = Motion { amount: cyclic amount, ring: RightRing }

r' :: Int -> Motion
r' amount = Motion { amount: cyclic $ negate amount, ring: RightRing }
