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
    let initialState = encode $ initialStore env

    storeTvar <- createRef r ("store" :: Text) $ newTVarIO initialState
    restart r "webserver" $ startServer env storeTvar


startServer :: Env -> TVar Lazy.ByteString -> IO ()
startServer env storeTvar = do
  let spa :: JSM ()
      spa = do
        setTitle "Gemini"
        addStyle "/public/styles/index.css"

        shpadoinkle identity runParDiff storeTvar (zoomComponent json rootView) stage


  let staticFolder = "./"
  liveWithStatic (env ^. #port) spa staticFolder

json :: Iso' Lazy.ByteString Store
json = iso fromBytes toBytes
  where
    toBytes :: Store -> Lazy.ByteString
    toBytes = encode

    fromBytes :: Lazy.ByteString -> Store
    fromBytes = (either (error . toText) identity . eitherDecode')


zoomComponent :: (Functor m, Is k A_Lens, Is k A_Getter) => Optic' k ix s a -> (a -> Html m a) -> (s -> Html m s)
zoomComponent optic component props = component (view optic props) & generalize optic
