{-# options_ghc -Wwarn #-}
module Gemini
  ( Store
  , initialState
  , rootView
  ) where

import           Relude

import           Data.Traversable          (for)
import           Optics                    hiding ((#))
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import           System.Random.Stateful    (globalStdGen, uniformM)

import           Shpadoinkle
import qualified Shpadoinkle.Html          as Html
import qualified Shpadoinkle.Keyboard      as Key
import           Shpadoinkle.Lens          (generalize)

import           Gemini.Html               (geminiHtmlView)
import           Gemini.Svg                (geminiSvgView)
import           Gemini.Types


-- | UI Operations
applyRotation :: Rotation -> Store -> Store
applyRotation r = over #gemini (rotate r) . over #history (r :)


initialState :: Store
initialState = Store
  { gemini = solvedGemini
  , history = []
  , options = Options
      { showLabels = False
      , animate = True
      , useSvg = False
      }
  }


-- | Components
rootView :: MonadIO m => Store -> Html m Store
rootView state =
  Html.div
    [ Html.className "gemini-app"
    , Html.onKeydown $ \key ->
      case key of
        -- the keyboard shortcuts are based on the top row of keys in the rightmost positions:
        -- T, Y, U, I, O, P
        Key.T -> applyRotation $ Rotation LeftRing Clockwise
        Key.Y -> applyRotation $ Rotation LeftRing AntiClockwise
        Key.U -> applyRotation $ Rotation CenterRing Clockwise
        Key.I -> applyRotation $ Rotation CenterRing AntiClockwise
        Key.O -> applyRotation $ Rotation RightRing Clockwise
        Key.P -> applyRotation $ Rotation RightRing AntiClockwise
        _     -> identity
    ]
    [ buttonsRow state
    , geminiView (state ^. #options) (state ^. #gemini)
    , debugView state
    ]
  where
    geminiView =
      if state ^. #options % #useSvg
      then geminiSvgView
      else geminiHtmlView



buttonsRow :: MonadIO m => Store -> Html m Store
buttonsRow state =
  Html.div
    [ Html.className "motion-buttons-row"
    ]
    [ Html.div
        [ Html.className "motion-buttons-group"
        ]
        ( scrambleButton
        : (buttonToggle ("Show Labels", "Hide Labels") & zoomComponent (#options % #showLabels) state)
        : (buttonToggle ("Animate", "Disable Animation") & zoomComponent (#options % #animate) state)
        --: (buttonToggle ("Use Svg", "Use Html") & zoomComponent (#options % #useSvg) state)
        : ( flip map rotations $ \rotation ->
              Html.button
                [ Html.className "motion-button"
                , Html.onClick $ applyRotation rotation
                ]
                [ Html.text $ prettyCompactText rotation
                ]
          )
        )
    ]
  where
    rotations =
      [ Rotation LeftRing Clockwise
      , Rotation LeftRing AntiClockwise
      , Rotation CenterRing Clockwise
      , Rotation CenterRing AntiClockwise
      , Rotation RightRing Clockwise
      , Rotation RightRing AntiClockwise
      ]


buttonToggle :: (Text, Text) -> Bool -> Html a Bool
buttonToggle (enable, disable) on =
  Html.button
    [ Html.className "toggle"
    , Html.onClick toggle
    ]
    [ Html.text $ if on then disable else enable
    ]
    where toggle = not


scrambleButton :: MonadIO m => Html m Store
scrambleButton =
  Html.button
    [ Html.onClickM $ do
        rotations <- for ([1..1000] :: [Int]) $ \_ -> (Endo . rotate) <$> uniformM globalStdGen
        let Endo scramble = fold rotations
        pure $ (set #history []) . (over #gemini scramble)
    ]
    [ Html.text "Scramble" ]

debugView :: Store -> Html m a
debugView state =
  Html.div
    [ Html.className "gemini-debug"]
    [ Html.text $ prettyCompactText (state ^. #history % to (take 10))
    ]


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


-- |
zoomComponent :: Functor m => Lens' s a -> s -> (a -> Html m a) -> Html m s
zoomComponent optic props component = component (props ^. optic) & generalizeOptic optic

generalizeOptic :: (Functor m, Continuous f) => Lens' s a -> (f m a -> f m s)
generalizeOptic optic = generalize $ toLensVL optic
