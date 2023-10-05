module Test.Gemini.LocationSpec where

import Gemini.Prelude

import Data.Gemini.Gen (AnyLocation(..))
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Location" $ do
    it "sibling" $ do
      sibling (location LeftRing 0) `shouldEqual` Nothing
      sibling (location LeftRing 2) `shouldEqual` Just (location CenterRing 16)

    it "sibling is its own (partial) inverse"
      $ quickCheck
      $ \(AnyLocation x) -> case sibling x of
          Nothing -> Success
          Just y -> Just x === sibling y
