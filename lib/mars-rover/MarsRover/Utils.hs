module MarsRover.Utils 
  ( throwLeft
  ) where

import Relude
import Control.Exception (throwIO)

throwLeft :: (MonadIO m, Exception e) => Either e a -> m a
throwLeft = liftIO . either throwIO pure
