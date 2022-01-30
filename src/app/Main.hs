module Main where


import           Relude

import           Shpadoinkle.Run  (runJSorWarp)

import           Gemini.App
import           Gemini.Views.App


main :: IO ()
main = do
  env@Env { port }  <- getEnv Prod
  putStrLn $ "Listening on port " <> show port
  runJSorWarp port $ app $ initialStore env
