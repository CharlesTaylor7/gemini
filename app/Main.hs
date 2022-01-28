module Main where


import           Relude

import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)

import           Gemini.Env
import           Gemini.Jsaddle
import           Gemini.Views.App



main :: IO ()
main = do
  env@Env { port }  <- getEnv Prod
  putStrLn $ "Listening on port " <> show port
  runJSorWarp port $ app $ initialStore env


dev :: IO ()
dev = do
  env@Env { port }  <- getEnv Dev
  let spa = app $ initialStore env
  let staticFolder = "./"
  liveWithStatic port spa staticFolder


app :: Store -> JSM ()
app store = do
  setTitle "Gemini"
  addStyle "/public/styles/index.css"
  simple runParDiff store rootView stage


getEnv :: Deployment -> IO Env
getEnv deployment = do
  port <- envOptional "PORT" 8000
  commit <- envOptional "COMMIT" "master"
  pure $ Env { deployment, port, commit }
