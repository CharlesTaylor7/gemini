module Main where


import           Relude

import           Language.Javascript.JSaddle (JSVal, MakeArgs, ToJSVal (..), fromJSValUnchecked, instanceOf, jsg, (!!),
                                              (!), (#), (<#))
import           Optics                      ((%), (.~), (^.))
import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)

import           Gemini.Env
import           Gemini.UI                   (AppEnv (..), Options (..), Store (..), initialStore, rootView)



main :: IO ()
main = do
  Env { port }  <- getEnv
  putStrLn $ "Listening on port " <> show port
  runJSorWarp port $ app $ initialStore Prod


dev :: IO ()
dev = do
  let initialPage = app $ initialStore Dev
  let staticFolder = "./"
  Env { port }  <- getEnv
  liveWithStatic port initialPage staticFolder


app :: Store -> JSM ()
app store = do
  setTitle "Gemini"
  addStyle "/public/styles/index.css"
  simple runParDiff store rootView stage


setTitle :: Text -> JSM ()
setTitle title = jsg ("document" :: Text) <# ("title" :: Text) $ title


data Env = Env
  { port   :: !Int
  , commit :: !Text
  }


getEnv :: IO Env
getEnv = do
  port <- envOptional "PORT" 8000
  commit <- envOptional "COMMIT" "master"
  pure $ Env { port, commit }
