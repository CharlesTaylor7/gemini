{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}
module Variant
  ( V(..), pattern V
  , errorToVariant
  , runErrorStdoutIOFinal
  , runErrorToExceptionIO
  ) where

import Relude
import Polysemy
import Polysemy.Error
import Haskus.Utils.Variant (V(..), pattern V, (:<))
import Control.Exception (throwIO)


errorToVariant
  :: forall e es r a.
    ( e :< es
    , Member (Error (V es)) r
    )
  => Sem (Error e : r) a -> Sem r a
errorToVariant = mapError V


runErrorStdoutIOFinal
  :: (Typeable e, Member (Final IO) r, Show e)
  => Sem (Error e : r) ()
  -> Sem r ()
runErrorStdoutIOFinal m = do
  maybeError <- errorToIOFinal m
  case maybeError of
    Left e -> embedFinal $ print e
    _      -> pass

runErrorToExceptionIO
  :: forall e m a r.
    (Exception e, MonadIO m, Member (Embed m) r)
  => Sem (Error e : r) a
  -> Sem r a
runErrorToExceptionIO m = 
  runError m >>= \case
    Left e  -> embed $ liftIO $ throwIO e
    Right a -> pure a

instance (Show (V es), Typeable (V es)) => Exception (V es)
