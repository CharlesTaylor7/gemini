module Data.Timestamp
  ( Timestamp(..)
  ) where

import           Relude

import           Data.Aeson    (FromJSON, ToJSON)
import           Prettyprinter (Pretty (..))
import qualified Prettyprinter as Pretty

-- | timestamp in milliseconds
newtype Timestamp = Timestamp { milliseconds :: Int }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty Timestamp where
  pretty (Timestamp milliseconds) = do
    let (seconds, ms) = milliseconds `divMod` 1000
        (minutes, s) = seconds `divMod` 60
        (hours, min) = minutes `divMod` 60

    zip [hours, min, s, ms] ["hours", "minutes", "seconds", "ms"]
    & filter ((>0) . fst)
    & map (\(x, unit) -> show x <> " " <> unit)
    & Pretty.sep

