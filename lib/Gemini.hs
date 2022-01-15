module Gemini
  ( Store
  , initialState
  , rootView
  ) where

import           Relude

import           Data.Finitary
import qualified Data.Sequence          as Seq
import qualified Data.Text              as Text
import           Data.Traversable       (for)
import           Optics                 hiding ((#))
import           Permutation
import           System.Random.Stateful (globalStdGen, uniformM)
import           Utils

import           Shpadoinkle
import qualified Shpadoinkle.Html       as Html
import qualified Shpadoinkle.Keyboard   as Key

import           Gemini.Html            (geminiHtmlView)
import           Gemini.Svg             (geminiSvgView)
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
      else const Seq.Empty


stopRecording :: Store -> Store
stopRecording state = state
  & #options % #recording .~ False
  & #moves %~ updateMoves
  & #history .~ Seq.Empty
  where
    updateMoves =
      case state ^. #history of
        Seq.Empty -> identity
        motions   -> (toMove motions :<|)


initialState :: Store
initialState = Store
  { gemini = initialGemini
  , history = Seq.Empty
  , moves = Seq.Empty
  , hovered = HoverState
    { activeCycle = Nothing
    , overMove = False
    }
  , options = Options
      { showLabels = False
      , animate = True
      , useSvg = False
      , recording = False
      , debug = False
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
    [ header state
    , Html.div
      [ Html.className "gemini-wrapper" ]
      [ geminiView state
      , savedMovesPanel state
      ]
    , if state ^. #options % #debug then debugView state else Html.span'_
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
        , ("padding", "12px")
        , ("display", "flex")
        , ("flex-direction", "column")
        ]
    ]
    [ Html.div_ [ Html.text $ "Recorded : " <> (prettyCompactText $ state ^.. #history % folded) ]
    ]

savedMovesPanel :: Store -> Html m Store
savedMovesPanel state =
  Html.div
    [ Html.className "saved-moves-panel" ]
    (  (itoListOf (#moves % ifolded) state <&> moveView)
    <> [ Html.div' [ Html.className "scrollbar-pad" ] ]
    )
  where
    moveView :: (Int, Move) -> Html m Store
    moveView (i, move) =
      Html.div
        [ Html.className "move" ]
        [ Html.div
          [ Html.className "move-description" ]
          [ Html.div
              [ Html.className "motions" ]
              [ Html.text $ (prettyCompactText $ move ^.. #motions % folded ) <> ":" ]
          , Html.div
              [ Html.className "cycles"
              , Html.onMouseenter $ #hovered % #overMove .~ True
              , Html.onMouseleave $ #hovered % #overMove .~ False
              ]
              ( move & moveCycles & uncycles & toList <&> \cycle ->
                  Html.div
                    [ Html.className "cycle"
                    , Html.onMouseenter $ #hovered % #activeCycle ?~ cycle
                    , Html.onMouseleave $ #hovered % #activeCycle .~ Nothing
                    ]
                    [ Html.text $ prettyCompactText $ cycle ]
              )
          ]
        , Html.button
            [ Html.className "delete-move"
            , Html.onClick $ #moves %~ Seq.deleteAt i
            ]
            [ Html.text $ "✕" ]
        ]


header :: MonadIO m => Store -> Html m Store
header state =
  Html.header
    [ Html.className "header"
    ]
    [ Html.div
      [ Html.className "control-panel"
      ]
      [ buttonGroup "options"
        [ (checkBox "Labels" & zoomComponent (#options % #showLabels) state)
        , (checkBox "Animate" & zoomComponent (#options % #animate) state)
        , (checkBox "Debug" & zoomComponent (#options % #debug) state)
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


actionButton :: [(Text, Prop m a)] -> [Html m a] -> Html m a
actionButton props = Html.button $ Html.className "action-button" : props


recordButton :: Store -> Html m Store
recordButton state = if state ^. #options % #recording then stopRecordingButton else startRecordingButton
  where
    startRecordingButton :: Html m Store
    startRecordingButton =
      actionButton
        [ Html.onClick $ #options % #recording .~ True ]
        [ Html.text $ "Start Recording" ]


    stopRecordingButton :: Html m Store
    stopRecordingButton =
      actionButton
        [ Html.onClick $ stopRecording ]
        [ Html.text $ "Stop Recording" ]


resetButton :: Html m Store
resetButton =
  actionButton
    [ Html.onClick $
        (#history .~ Seq.Empty) .
        (#options % #recording .~ False) .
        (#gemini .~ initialGemini)
    ]
    [ Html.text "Reset" ]



scrambleButton :: forall m. MonadIO m => Html m Store
scrambleButton =
  actionButton
    [ Html.onClickM $ do
        rotations <- for ([1..1000] :: [Int]) $ \_ -> (Endo . rotate) <$> uniformM globalStdGen
        let Endo scramble = fold rotations
        pure $ (set #history Seq.Empty) . (over #gemini scramble)
    ]
    [ Html.text "Scramble" ]
