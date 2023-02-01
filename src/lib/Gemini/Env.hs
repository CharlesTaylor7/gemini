module Gemini.Env
  ( EnvVarException(..)
  , envOptional, envRequired
  ) where

import           Control.Exception  (throwIO)
import           Relude
import           System.Environment (lookupEnv)


data EnvVarException = Unset String | InvalidFormat String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)


-- | Provide additional context to the failure case by upgrading a Maybe to an Either
note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e _        = Left e


envOptional :: Read a => String -> a -> IO a
envOptional var def = do
  maybeVar <- lookupEnv var
  pure $ fromMaybe def $ do
    var <- maybeVar
    readMaybe var


envRequired :: Read a => String -> IO a
envRequired var = do
  maybeVar <- lookupEnv var
  either throwIO pure $ do
    var <- maybeVar & note (Unset var)
    readMaybe var & note (InvalidFormat var)
