module Gemini.Types where

import           Relude

import qualified Data.Text as Text


-- | Definitions
newtype Gemini = Gemini (Map Location Disk)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)



newtype Location = Location Int
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)


data Disk = Disk
  { color :: !Color
  , label :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)



data Color
  = White
  | Yellow
  | Black
  | Red
  | Green
  | Blue
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)


-- | Basic operations
allDisks :: [Disk]
allDisks
  =  diskSet White 9
  <> diskSet Yellow 9
  <> diskSet Black 8
  <> diskSet Red 8
  <> diskSet Green 8
  <> diskSet Blue 8
  where
    diskSet :: Color -> Int -> [Disk]
    diskSet color n = map (\c -> Disk color $ Text.singleton c) (take n ['A'..])


solvedGemini :: Gemini
solvedGemini = Gemini $ fromList []
