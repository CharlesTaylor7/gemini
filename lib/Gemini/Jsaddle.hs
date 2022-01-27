module Gemini.Jsaddle
  ( jsCall, jsConsoleLog, setTitle
  -- reexports
  , JSM, JSVal, ToJSVal(..), FromJSVal(..), MakeArgs
  , jsg, instanceOf
  , (!), (!!)
  ) where

import           Relude

import           Language.Javascript.JSaddle (FromJSVal (..), JSM, JSVal, MakeArgs, ToJSVal (..), instanceOf, jsg, (!!),
                                              (!), (#), (<#))

jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text


jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args


setTitle :: Text -> JSM ()
setTitle title = jsg ("document" :: Text) <# ("title" :: Text) $ title
