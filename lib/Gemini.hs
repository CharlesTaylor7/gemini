module Gemini
  ( Store
  , initialState
  , rootView
  ) where

import           Data.Finitary
import qualified Data.InsertionOrdSet      as InsertionOrder
import qualified Data.Text                 as Text
import           Data.Traversable          (for)
import           Optics                    hiding ((#))
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import           Relude
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
applyRotation rotation state = state
  & #gemini %~ rotate rotation
  & #history %~ updateHistory
  where
    updateHistory =
      if state ^. #options % #recording
      then continueMotion rotation
      else const EmptySequence


stopRecording :: Store -> Store
stopRecording state = state
  & #options % #recording .~ False
  & #moves %~ updateMoves
  & #history .~ EmptySequence
  where
    updateMoves =
      case state ^. #history of
        EmptySequence -> identity
        motions       -> InsertionOrder.insert (toMove motions)


initialState :: Store
initialState = Store
  { gemini = initialGemini
  , history = EmptySequence
  , moves = InsertionOrder.empty
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
    , savedMovesPanel state
    ]
  where
    geminiView =
      if state ^. #options % #useSvg
      then geminiSvgView
      else geminiHtmlView


debugView :: Store -> Html m a
debugView state =
  Html.div
    [ Html.styleProp
        [ ("position", "absolute")
        , ("top", "0")
        , ("left", "0")
        , ("display", "flex")
        , ("flex-direction", "column")
        ]
    ]
    [ Html.div_ [ Html.text $ "Recorded : " <> (prettyCompactText $ state ^.. #history % folded) ]
    , Html.div_ [ Html.text $ "Moves : " <> (prettyCompactText $ state ^.. #moves % folded) ]
    ]

savedMovesPanel :: Store -> Html m Store
savedMovesPanel state =
  Html.div
    [ Html.className "saved-moves"
    ]
    ( itoListOf (#moves % #intMap % ifolded) state <&> moveView )
  where
    moveView :: (Int, Move) -> Html m Store
    moveView (i, move) =
      Html.div
        [ ]
        [ Html.button
          [ Html.onClick $ #moves %~ InsertionOrder.delete i ]
          [ Html.text $ "delete" ]
        ]


controlPanel :: MonadIO m => Store -> Html m Store
controlPanel state =
  Html.div
    [ Html.className "control-panel-wrapper"
    ]
    [ Html.div
      [ Html.className "control-panel"
      ]
      [ buttonGroup "options"
        [ (checkBox "Labels" & zoomComponent (#options % #showLabels) state)
        , (checkBox "Animate" & zoomComponent (#options % #animate) state)
        -- , (checkBox "Svg" & zoomComponent (#options % #useSvg) state)
        ]
      , buttonGroup "actions"
        [ resetButton
        , scrambleButton
        , recordButton state
        ]
      ]
  ]


buttonGroup :: Text -> [Html m a] -> Html m a
buttonGroup className = Html.div [ Html.class' ["button-group", className] ]

motionButtons :: Html m Store
motionButtons =
  buttonGroup "motions"
    ( flip map inhabitants $ \rotation ->
        Html.button
          [ Html.className "motion-button"
          , Html.onClick $ applyRotation rotation
          ]
          [ Html.text $ prettyCompactText rotation
          ]
    )


checkBox :: Text -> Bool -> Html a Bool
checkBox label checked =
  Html.label
    [ Html.className "checkbox" ]
    [ Html.span
        [ Html.className "checkbox-label" ]
        [ Html.text label ]
    , Html.input'
        [ Html.type' "checkbox"
        , Html.checked checked
        , Html.onCheck const
        ]
    ]


recordButton :: Store -> Html m Store
recordButton state = if state ^. #options % #recording then stopRecordingButton else startRecordingButton
  where
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


resetButton :: Html m Store
resetButton =
  Html.button
    [ Html.onClick $
        (#history .~ EmptySequence) .
        (#options % #recording .~ False) .
        (#gemini .~ initialGemini)
    ]
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
