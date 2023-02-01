module Gemini.temp
  ( -- jsCall
  onResize, setTitle, sleep, dateNow, jsCall, jsGlobal
  -- reexports
  , JSM, JSVal, ToJSVal(..), FromJSVal(..), MakeArgs
  , jsg, instanceOf
  , (!), (!!)
  ) where

import           Relude

import           Control.Concurrent          (threadDelay)
import           Data.Timestamp              (Timestamp (..))
import           Language.Javascript.JSaddle (FromJSVal (..), jsg, JSM, JSVal, MakeArgs, MonadJSM (..), ToJSVal (..), eval, function,
                                              instanceOf, liftJSM, (!!), (!), (#), (<#))


jsGlobal :: Text -> JSM JSVal
jsGlobal = jsg

jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args


dateNow :: MonadJSM m => m Timestamp
dateNow = liftJSM $ Timestamp <$> (eval ("Date.now()" :: Text) >>= fromJSValUnchecked)


setTitle :: Text -> JSM ()
setTitle title = jsGlobal "document" <# ("title" :: Text) $ title


sleep :: forall m. MonadJSM m => Int -> m ()
sleep seconds = liftIO $ threadDelay microseconds
  where
    microseconds = seconds * 1000000

onResize :: MonadJSM m => JSM () -> m ()
onResize callback = void $ liftJSM $ resize
  where
    resize :: JSM JSVal
    resize = jsGlobal "window" `jsCall` "addEventListener" $ ("resize" :: Text, fn)
    fn = function \_ _ _ -> callback
