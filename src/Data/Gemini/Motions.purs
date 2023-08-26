module Data.Gemini.Motions where

import Data.Gemini
import Data.Cyclic (cyclic)

l :: Int -> Motion
l amount = Motion { amount: cyclic amount, rotation: rotation LeftRing Clockwise}

l' :: Int -> Motion
l' amount = Motion { amount: cyclic amount, rotation: rotation LeftRing AntiClockwise}

c :: Int -> Motion
c amount = Motion { amount: cyclic amount, rotation: rotation CenterRing Clockwise }

c' :: Int -> Motion
c' amount = Motion { amount: cyclic amount, rotation: rotation CenterRing AntiClockwise }
  
r :: Int -> Motion
r amount = Motion { amount: cyclic amount, rotation: rotation RightRing Clockwise }

r' :: Int -> Motion
r' amount = Motion { amount: cyclic amount, rotation: rotation RightRing AntiClockwise }
