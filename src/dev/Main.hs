{-# OPTIONS_GHC -Wwarn #-}
module Main where

import           Relude

import           Shpadoinkle                 (JSM, shpadoinkle)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             hiding (Dev)

import           Rapid

import           Gemini.App                  hiding (app)
import           Gemini.Views.App


main :: IO ()
main =
  rapid 0 $ \r -> do
    serializedStore :: TVar Text <- createRef r "store" $ newEmptyTVarIO
    restart r "webserver" $ startServer serializedStore


startServer :: TMVar Text -> IO ()
startServer tvar = do
  env@Env { port }  <- getEnv Dev

  let spa :: JSM ()
      spa = do
        setTitle "Gemini"
        addStyle "/public/styles/index.css"
        shpadoinkle identity runParDiff tvar _ _

  -- runParDiff store rootView stage

  let staticFolder = "./"
  liveWithStatic port spa staticFolder




