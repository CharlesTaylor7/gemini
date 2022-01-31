{-# OPTIONS_GHC -Wwarn #-}
module Main where

import           Relude

import           Data.Aeson                  hiding (json)
import qualified Data.ByteString.Lazy        as Lazy
import           Optics

import           Shpadoinkle                 (JSM, shpadoinkle)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             hiding (Dev, Env)

import           Rapid

import           Gemini.App                  hiding (app)
import           Gemini.Utils                (generalize)
import           Gemini.Views.App


main :: IO ()
main =
  rapid 0 $ \r -> do
    env <- createRef r ("env" :: Text) $ getEnv Dev
    let store = initialStore env
    storeTVar <- createRef r ("store" :: Text) $ newTVarIO (store ^. re json)
    recordedMoves <- createRef r ("recorded" :: Text) $ Lazy.readFile "./recorded-moves.txt"

    restart r "webserver" $ do
      readTVarIO storeTVar
      let spa :: JSM ()
          spa = do
            setup
            shpadoinkle identity runParDiff storeTVar (zoomComponent (json `withDefault` store) rootView) stage

      let staticFolder = "./"
      liveWithStatic (env ^. #port) spa staticFolder


setup :: JSM ()
setup = do
  setTitle "Gemini"
  addStyle "/public/styles/index.css"


json :: forall a. (FromJSON a, ToJSON a) => Prism' Lazy.ByteString a
json = prism' encode decode'

-- lawless, but for the same reasons as `non`. Should be harmless
withDefault :: Prism' s a -> a -> Iso' s a
withDefault prism def = iso (fromMaybe def . preview prism ) (review prism)


zoomComponent :: (Functor m, Is k A_Lens, Is k A_Getter) => Optic' k ix s a -> (a -> Html m a) -> (s -> Html m s)
zoomComponent optic component props = component (view optic props) & generalize optic
