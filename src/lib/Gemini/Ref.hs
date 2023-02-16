-- | A global ref
module Gemini.Ref
  ( updateStore
  , initGlobalRef
  )
  where

import           Relude


import           Gemini.Types     (Store)
import           Shpadoinkle      (JSM, MonadJSM, liftJSM)
import           System.IO.Unsafe (unsafePerformIO)

type F = Store -> Store
type Handler = F -> JSM ()

ref :: IORef (Maybe Handler)
ref = unsafePerformIO $ newIORef Nothing
{-# NOINLINE ref #-}

initGlobalRef :: MonadJSM m => Handler -> m ()
initGlobalRef = liftJSM . writeIORef ref . Just

updateStore :: MonadJSM m => F -> m ()
updateStore f = liftJSM $ do
  Just h <- readIORef ref
  h f
