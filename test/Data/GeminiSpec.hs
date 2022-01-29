module Data.GeminiSpec where

import           Relude

import           Data.Finitary

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.Cyclic
import           Data.Gemini


instance Arbitrary Location where
  arbitrary = elements inhabitants


spec :: Spec
spec = do
  describe "Gemini" $ do
    it "isSolved" $ do
      initialGemini `shouldSatisfy` isSolved
      (initialGemini & applyToGemini (Rotation LeftRing Clockwise)) `shouldNotSatisfy` isSolved

  describe "Location" $ do
    it "sibling" $ do
      sibling (Location LeftRing 0) `shouldBe` Nothing
      sibling (Location LeftRing 2) `shouldBe` Just (Location CenterRing 16)

    prop "sibling is its own (partial) inverse" $ do
      \x -> case sibling x of
        Nothing -> pure ()
        Just y  -> sibling y `shouldBe` Just x
