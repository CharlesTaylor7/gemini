{-# OPTIONS_GHC -Wwarn #-}
module Main where

import           Relude

import           Data.Aeson                  hiding (json)
import qualified Data.ByteString.Lazy        as Lazy
import           Optics

import           Shpadoinkle                 (JSM, shpadoinkle, MonadJSM, Html)
import qualified Shpadoinkle.Html  as Html
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage, ParDiffT)
import qualified Shpadoinkle.Run            as Run

import           Rapid

import           Gemini.App                  (getEnv)
import           Gemini.Utils                (IsLens, zoomComponent)
import           Gemini.Views.App           (Store(..), Env(..), rootView, initialStore, Deployment(..))
import           Gemini.Views.Puzzle (loadDomInfo)
import           Gemini.FFI (onResize)


main :: IO ()
main =
  rapid 0 $ \r -> do

    store <- createRef r ("initialStore" :: Text) $ do
      env <- getEnv Dev
      pure $ initialStore env

    storeTVar <- createRef r ("storeTVar" :: Text) $ newTVarIO (store ^. re json)

    restart r "webserver" $ do
      let jsonIso = json `withDefault` store
      let staticFolder = "./"
      Run.liveWithStatic (store ^. #env % #port) (spa jsonIso storeTVar rootView) staticFolder


spa :: forall k ix m
    . IsLens k
    => MonadJSM m
    => m ~ (Shpadoinkle.Backend.ParDiff.ParDiffT Lazy.ByteString JSM)
    => Optic' k ix Lazy.ByteString Store
    -> TVar Lazy.ByteString
    -> (Store -> Html m Store)
    -> JSM ()
spa lens storeTVar rootView = do
  Html.setTitle "Gemini"
  Html.addStyle "/public/styles/index.css"

  let modify = atomically . modifyTVar' storeTVar . over lens
  onResize $ do
    domInfo <- loadDomInfo
    modify $ #dom .~ domInfo

  let view props = zoomComponent lens props rootView
  shpadoinkle identity runParDiff storeTVar view stage


json :: forall a. (FromJSON a, ToJSON a) => Prism' Lazy.ByteString a
json = prism' encode decode'

-- lawless, but for the same reasons as `non`. Should be harmless
withDefault :: Prism' s a -> a -> Iso' s a
withDefault prism def = iso (fromMaybe def . preview prism ) (review prism)
