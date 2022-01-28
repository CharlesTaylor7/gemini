module Gemini.Jsaddle
  ( jsCall, jsConsoleLog, setTitle, sleep
  -- reexports
  , JSM, JSVal, ToJSVal(..), FromJSVal(..), MakeArgs
  , jsg, instanceOf
  , (!), (!!)
  ) where

import           Relude

import           Language.Javascript.JSaddle (FromJSVal (..), Function, JSM, JSVal, MakeArgs, ToJSVal (..),
                                              asyncFunction, eval, function, instanceOf, jsg, jsgf, (!!), (!), (#),
                                              (<#))


jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text


jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args


setTitle :: Text -> JSM ()
setTitle title = jsg ("document" :: Text) <# ("title" :: Text) $ title

newtype Promise = Promise JSVal


sleep :: Int -> JSM Promise
sleep ms = Promise <$> eval ("new Promise(resolve => setTimeout(resolve, " <> show ms <> "))" :: Text)

pThen :: Promise -> Function -> JSM Promise
pThen (Promise jsVal) continuation = do
  f <- toJSVal continuation
  Promise <$> jsCall jsVal "then" f
