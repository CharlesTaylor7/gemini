module Main where

import           Control.Exception           (throwIO)
import           Language.Javascript.JSaddle (JSVal, MakeArgs, ToJSVal (..), fromJSValUnchecked, instanceOf, jsg, (!!),
                                              (!), (#), (<#))
import           Optics                      ((%), (.~), (^.))
import           Relude
import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)
import           System.Environment          (lookupEnv)

import           Gemini                      (AppEnv (..), Options (..), Store (..), initialStore, rootView)
import           Gemini.Env



main :: IO ()
main = do
  Env { port }  <- getEnv
  putStrLn $ "Listening on port " <> show port
  runJSorWarp port $ app $ initialStore Prod


dev :: IO ()
dev = do
  let initialPage = app $ initialStore Dev
  let staticFolder = "public/"
  Env { port }  <- getEnv
  liveWithStatic port initialPage staticFolder


app :: Store -> JSM ()
app store = do
  match <- jsg ("window" :: Text) # ("matchMedia" :: Text) $ ("only screen and (max-width: 760px)" :: Text)
  isMobile <- match ! ("matches" :: Text) >>= fromJSValUnchecked

  let store' = store & #options % #isMobile .~ isMobile

  -- set page title
  jsg ("document" :: Text) <# ("title" :: Text) $ ("Gemini" :: Text)
  addStyle cssStylePath
  simple runParDiff store' rootView stage
    where
      cssStylePath =
        case store ^. #options % #isProd of
          True  -> "/public/styles/index.css"
          False -> "./styles/index.css"


data Env = Env
  { port   :: !Int
  , commit :: !Text
  }


getEnv :: IO Env
getEnv = do
  port <- envOptional "PORT" 8000
  commit <- envOptional "COMMIT" "master"
  pure $ Env { port, commit }
