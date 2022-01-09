{-# options_ghc -Wwarn #-}
module PhotoBrowser where

import           Relude                     hiding (Option)

import           Control.Exception          (throwIO)
import           Data.List                  (findIndex)
import           Data.Text                  (pack, unpack)
import           Data.Vector                (Vector)
import           Optics
import           Rel8                       (Id (..))

import           Shpadoinkle
import qualified Shpadoinkle.Continuation   as Continuation
import qualified Shpadoinkle.Html           as Html
import qualified Shpadoinkle.Keyboard       as Key

import           MarsRover.PhotoBrowser.Api (Camera (..), Photo (..), Rover (..), RoverName, getPhotos, runHandler)


data Store = Store
  { rovers         :: ![Rover]
  , photos         :: ![Photo]
  , selectedRover  :: !(Maybe Rover)
  , selectedCamera :: !(Maybe Camera)
  , photoIndex     :: !Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

selectedRoverCameras :: Store -> Vector Camera
selectedRoverCameras = fromMaybe mempty . preview (#selectedRover % _Just % #cameras)


initialState :: Store
initialState = Store
  { rovers = []
  , photos = []
  , selectedRover = Nothing
  , selectedCamera = Nothing
  , photoIndex = 0
  }


-- | state functions
movePrevPhoto, moveNextPhoto :: Store -> Store
movePrevPhoto state =
  state & #photoIndex %~ (max 0 . subtract 1)

moveNextPhoto state@Store { photos } =
  state & #photoIndex %~ (min n . (+1))
  where
    n = max 0 (length photos - 1)

selectedPhoto :: Store -> Maybe Photo
selectedPhoto Store { photos, photoIndex } = photos ^? ix photoIndex


rootView :: MonadIO m => Component m Store
rootView state =
  Html.div
    [ Html.className "slideshow"
    , Html.tabIndex 0
    , Html.onKeydownC \case
      Key.LeftArrow  -> Pure movePrevPhoto
      Key.RightArrow -> Pure moveNextPhoto
      _              -> done
    ]
    [ Html.div
      [ Html.className "control-panel" ]
      [ labeledDropdown DropdownProps
          { label = "Rover"
          , elements = toList $ state ^. #rovers
          , renderOption = \rover -> Option (rover ^. #id % to show) (rover ^. #name % to show)
          , handler = \rover -> Continuation.pur $ (#selectedRover .~ rover) . (#selectedCamera .~ Nothing)
          } $ state ^. #selectedRover
      , labeledDropdown DropdownProps
          { label = "Camera"
          , elements = toList $ selectedRoverCameras state
          , renderOption = \camera -> Option (camera ^. #id % to show) (camera ^. #name)
          , handler = \camera ->
            Continuation
              (#selectedCamera .~ camera)
              (\store -> liftIO $ do
                photos <- runHandler $ getPhotos
                  (store ^? #selectedRover % _Just % #id % to Id)
                  (store ^? #selectedCamera % _Just % #id % to Id)
                photos <- either throwIO pure photos
                pure $
                  Continuation.pur
                    (#photos .~ photos)
              )
          } $ state ^. #selectedCamera
      , Html.div
        [ Html.className "navigation"]
        [ arrowButton Prev state
        , arrowButton Next state
        ]
      ]
    , photoImg $ selectedPhoto state
    ]

data Direction = Prev | Next

arrowButton :: Direction -> Component m Store
arrowButton Prev state =
  Html.button
    [ Html.className "arrow-button"
    , Html.type' "button"
    , Html.disabled $
        let Store { photoIndex } = state
        in photoIndex == 0
    , Html.onClick movePrevPhoto
    ]
    [ "<" ]
arrowButton Next state =
  Html.button
    [ Html.className "arrow-button"
    , Html.type' "button"
    , Html.disabled $
        let Store { photos, photoIndex } = state
            n = length photos
        in photoIndex >= n - 1
    , Html.onClick moveNextPhoto
    ]
    [ ">" ]


photoImg :: Maybe Photo -> Html m a
photoImg maybePhoto =
  Html.img'
    [ Html.className "rover-photo"
    , maybe invisible (Html.src . view #imgSrc) maybePhoto
    ]
  where
    invisible = Html.styleProp [("display", "none")]


debugView state =
  Html.div_
    [ Html.br'_
    , Html.br'_
    , Html.br'_
    , Html.text $ show state
    ]


data Option = Option
  { value   :: !Text
  , display :: !Text
  }
  deriving stock (Eq, Generic)


data DropdownProps m a r = DropdownProps
  { label        :: Text
  , renderOption :: a -> Option
  , elements     :: [a]
  , handler      :: Maybe a -> Continuation m r
  }


labeledDropdown :: forall a r m. Functor m => DropdownProps m a r -> Maybe a -> Html m r
labeledDropdown DropdownProps { label, renderOption, elements, handler } current =
  Html.div
    [ Html.className "dropdown"]
    [ Html.div
        [ Html.className "dropdown-label"]
        [ Html.text $ label <> ":" ]
    , selectElement renderOption elements handler current
    ]


-- | A select component with the selected element as the state
selectElement :: forall a r m. Functor m => (a -> Option) -> [a] -> (Maybe a -> Continuation m r) -> Maybe a -> Html m r
selectElement toOption elements continuation current =
  let
    options :: [Option]
    options = map toOption elements

    toOptionValue :: a -> Text
    toOptionValue = view #value . toOption

    lookup :: Text -> Maybe a
    lookup value = find predicate elements
      where predicate = (== value) . toOptionValue
  in
    selectOption options (continuation . lookup) (maybe "" toOptionValue current)


-- | Basic select component that takes a continuation event handler
selectOption :: [Option] -> (Text -> Continuation m r) -> Text -> Html m r
selectOption options continuation current =
  Html.select
    [ Html.className "dropdown-select"
    , Html.required True
    , Html.onInputC continuation
    ]
    (zipWith renderOption baseProps options')

  where

    placeholder :: Option
    placeholder = Option { value = "", display = "Select an option" }

    options' = placeholder : options

    baseProps = replicate (length options') []
      & ix 0 .~ [Html.disabled True]
      & ix selectedIndex %~ select

    selectedIndex :: Int
    selectedIndex = fromMaybe 0 $ findIndex predicate options'
      where predicate = (== current) . view #value

    select :: PropList m a -> PropList m a
    select = ((Html.selected True) :)

    renderOption :: PropList m a -> Option -> Html m a
    renderOption props Option { display, value } =
      Html.option
        (Html.value value : props)
        [Html.text display]


-- | Type aliases
type PropList m a = [Property m a]
type Property m a = (Text, Prop m a)
type Component monad state = state -> Html monad state
