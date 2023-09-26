module Test.Util where

import Prelude

import Data.Permutation (Permutation)
import Effect.Class (liftEffect)
import Test.Gemini.Gen (AnyPermutation(..))
import Test.QuickCheck (class Testable, Result, quickCheck, (===))
import Test.Spec (Spec, it)

prop :: forall a. Testable a => String -> a -> Spec Unit
prop testCase body = it testCase $ liftEffect $ quickCheck body
