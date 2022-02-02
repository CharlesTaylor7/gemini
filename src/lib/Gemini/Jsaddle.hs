module Gemini.Jsaddle
  ( jsCall, jsConsoleLog, setTitle, sleep, dateNow
  -- reexports
  , JSM, JSVal, ToJSVal(..), FromJSVal(..), MakeArgs
  , jsg, instanceOf
  , (!), (!!)
  ) where

import           Relude

import           Control.Concurrent          (threadDelay)
import           Gemini.Types                (Timestamp (..))
import           Language.Javascript.JSaddle (FromJSVal (..), JSM, JSVal, MakeArgs, MonadJSM (..), ToJSVal (..), eval,
                                              instanceOf, jsg, liftJSM, (!!), (!), (#), (<#))

dateNow :: MonadJSM m => m Timestamp
dateNow = liftJSM $ Timestamp <$> (eval ("Date.now()" :: Text) >>= fromJSValUnchecked)

jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text


jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args


setTitle :: Text -> JSM ()
setTitle title = jsg ("document" :: Text) <# ("title" :: Text) $ title


sleep :: forall m. MonadJSM m => Int -> m ()
sleep seconds = liftIO $ threadDelay microseconds
  where
    microseconds = seconds * 1000000
