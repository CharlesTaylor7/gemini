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
import           Gemini.Types
import           Gemini.Utils                (generalize)
import           Gemini.Views.App


main :: IO ()
main =
  rapid 0 $ \r -> do

    store <- createRef r ("initialStore" :: Text) $ do
      env <- getEnv Dev
      moves <- Lazy.readFile "./recorded-moves.txt" <&> view (json `withDefault` mempty)
      pure $ (initialStore env) { moves }

    storeTVar <- createRef r ("storeTVar" :: Text) $ newTVarIO (store ^. re json)

    restart r "webserver" $ do
      let jsonIso = json `withDefault` store

      moves <- readTVarIO storeTVar <&> view (jsonIso % #moves % re json)
      Lazy.writeFile "./recorded-moves.txt" moves

      let staticFolder = "./"
      liveWithStatic (store ^. #env % #port) (spa jsonIso storeTVar) staticFolder


spa :: (Is k A_Lens, Is k A_Getter) => Optic' k ix Lazy.ByteString Store -> TVar Lazy.ByteString -> JSM ()
spa lens storeTVar = do
  setTitle "Gemini"
  addStyle "/public/styles/index.css"
  let view = zoomComponent lens rootView
  shpadoinkle identity runParDiff storeTVar view stage


json :: forall a. (FromJSON a, ToJSON a) => Prism' Lazy.ByteString a
json = prism' encode decode'

-- lawless, but for the same reasons as `non`. Should be harmless
withDefault :: Prism' s a -> a -> Iso' s a
withDefault prism def = iso (fromMaybe def . preview prism ) (review prism)


zoomComponent :: (Functor m, Is k A_Lens, Is k A_Getter) => Optic' k ix s a -> (a -> Html m a) -> (s -> Html m s)
zoomComponent optic component props = component (view optic props) & generalize optic
