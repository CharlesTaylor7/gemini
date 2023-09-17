module LocationSpec where

import Gemini.Prelude

import Data.Location (indexToLocation, location, sibling)
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

newtype AnyLocation = AnyLocation Location

instance Arbitrary AnyLocation where
  arbitrary = AnyLocation <<< indexToLocation <$> Gen.chooseInt 0 53

locationSpec :: Spec Unit
locationSpec = do
  describe "Location" $ do
    it "sibling" $ do
      sibling (location LeftRing 0) `shouldEqual` Nothing
      sibling (location LeftRing 2) `shouldEqual` Just (location CenterRing 16)

    it "sibling is its own (partial) inverse"
      $ quickCheck
      $ \(AnyLocation x) -> case sibling x of
          Nothing -> Success
          Just y -> Just x === sibling y
