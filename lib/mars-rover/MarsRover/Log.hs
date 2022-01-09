module MarsRover.Log
  ( Log
  , log
  , runLogDebugIO
  ) where

import Relude
import Polysemy

import Colog (LoggerT, cmap, usingLoggerT, Message, fmtMessage, logDebug, logTextStdout)
import Colog.Polysemy (log)
import qualified Colog.Polysemy as Colog


type Log = Colog.Log Text


-- | effect handler interprets commands in Hasql db session via rel8
runLogDebugIO :: Member (Embed IO) r => Sem (Log : r) () -> Sem r ()
runLogDebugIO = interpret $ \case
  Colog.Log text -> prettyLog . logDebug $ text


prettyLog :: MonadIO m => LoggerT Message m a -> m a
prettyLog = usingLoggerT action
  where action = cmap fmtMessage logTextStdout
