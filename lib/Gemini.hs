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
import qualified Shpadoinkle.Lens

import           Gemini.Html               (geminiHtmlView)
import           Gemini.Svg                (geminiSvgView)
import           Gemini.Types


-- | UI Operations
applyRotation :: Rotation -> Store -> Store
applyRotation r state = state
  { gemini = state ^. #gemini % to (rotate r)
  , history
  }
  where
    history =
      if state ^. #options % #recording
      then state ^. #history % to ( :|> r)
      else EmptySequence


stopRecording :: Store -> Store
stopRecording = error "todo"


initialState :: Store
initialState = Store
  { gemini = initialGemini
  , history = EmptySequence
  , moves = EmptySequence
  , options = Options
      { showLabels = False
      , animate = True
      , useSvg = False
      , recording = False
      }
  }


-- | Components
rootView :: MonadIO m => Store -> Html m Store
rootView state =
  Html.div
    [ Html.className "gemini-app"
    , Html.tabIndex 0
    , Html.onKeydown $ \key ->
      case key of
        -- the keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
        -- T, Y, U, I, O, P
        Key.Q -> applyRotation $ Rotation LeftRing AntiClockwise
        Key.W -> applyRotation $ Rotation LeftRing Clockwise
        Key.T -> applyRotation $ Rotation CenterRing AntiClockwise
        Key.Y -> applyRotation $ Rotation CenterRing Clockwise
        Key.O -> applyRotation $ Rotation RightRing AntiClockwise
        Key.P -> applyRotation $ Rotation RightRing Clockwise
        _     -> identity
    ]
    [ controlPanel state
    , geminiView (state ^. #options) (state ^. #gemini)
    --, debugView state
    ]
  where
    geminiView =
      if state ^. #options % #useSvg
      then geminiSvgView
      else geminiHtmlView


controlPanel :: MonadIO m => Store -> Html m Store
controlPanel state =
  Html.div
    [ Html.className "control-panel"
    ]
    ( Html.div
        [ Html.className "button-group"
        ]
        [ (checkBox "Labels" & zoomComponent (#options % #showLabels) state)
        , (checkBox "Animate" & zoomComponent (#options % #animate) state)
        -- , (checkBox "Svg" & zoomComponent (#options % #useSvg) state)
        , resetButton
        , scrambleButton
        , recordButton state
        ]
    : ( flip map [Clockwise, AntiClockwise] $ \direction ->
          Html.div
            [ Html.className "button-group"
            ]
            ( flip map rings $ \ring ->
                let rotation = Rotation ring direction
                in Html.button
                    [ Html.className "motion-button"
                    , Html.onClick $ applyRotation rotation
                    ]
                    [ Html.text $ prettyCompactText rotation
                    ]
            )
      )
    )
  where
    rings = [LeftRing, CenterRing, RightRing]

checkBox :: Text -> Bool -> Html a Bool
checkBox label checked =
  Html.label_
    [ Html.span
        [ Html.className "label-text" ]
        [ Html.text label ]
    , Html.input'
        [ Html.className "checkbox"
        , Html.type' "checkbox"
        , Html.checked checked
        , Html.onCheck const
        ]
    ]

recordButton :: Store -> Html m Store
recordButton state = if state ^. #options % #recording then stopRecordingButton else startRecordingButton

startRecordingButton :: Html m Store
startRecordingButton =
  Html.button
    [ Html.onClick $ #options % #recording .~ True ]
    [ Html.text $ "Start Recording" ]


stopRecordingButton :: Html m Store
stopRecordingButton =
  Html.button
    [ Html.onClick $ stopRecording ]
    [ Html.text $ "Stop Recording" ]



buttonToggle (enable, disable) on =
  Html.button
    [ Html.className "toggle"
    , Html.onClick toggle
    ]
    [ Html.text $ if on then disable else enable
    ]
    where toggle = not


resetButton :: Html m Store
resetButton =
  Html.button
    [ Html.onClick $ const initialState]
    [ Html.text "Reset" ]



scrambleButton :: MonadIO m => Html m Store
scrambleButton =
  Html.button
    [ Html.onClickM $ do
        rotations <- for ([1..1000] :: [Int]) $ \_ -> (Endo . rotate) <$> uniformM globalStdGen
        let Endo scramble = fold rotations
        pure $ (set #history EmptySequence) . (over #gemini scramble)
    ]
    [ Html.text "Scramble" ]


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


-- |
zoomComponent :: Functor m => Lens' s a -> s -> (a -> Html m a) -> Html m s
zoomComponent optic props component = component (props ^. optic) & generalize optic

generalize :: (Functor m, Continuous f) => Lens' s a -> (f m a -> f m s)
generalize optic = Shpadoinkle.Lens.generalize $ toLensVL optic
