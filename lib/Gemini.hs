module Gemini
  ( Store
  , initialState
  , rootView
  ) where

import           Relude

import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence          as Seq
import           Data.Traversable       (for)
import           Optics                 hiding ((#))
import           System.Random.Stateful (globalStdGen, uniformM)
import           Utils

import           Shpadoinkle
import qualified Shpadoinkle.Html       as Html
import qualified Shpadoinkle.Keyboard   as Key

import           Gemini.Html            (geminiHtmlView)
import           Gemini.Svg             (geminiSvgView)
import           Gemini.Types
import           Gemini.UI.Actions


-- | Initial state of the app
initialState :: Store
initialState = Store
  { gemini = initialGemini
  , history = Seq.Empty
  , moves = Seq.Empty
  , hover = HoverState
    { activeCycle = Nothing
    , overMove = False
    }
  , drag = Nothing
  , options = Options
      { showLabels = False
      , animate = True
      , useSvg = False
      , recording = False
      , debug = False
      }
  , debugLog = ""
  }

applyRotation r = applyMotionToStore $ toMotion r

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
    ]
  where
    geminiView =
      if state ^. #options % #useSvg
      then geminiSvgView
      else geminiHtmlView


debugView :: Store -> Html m a
debugView state =
  if state ^. #options % #debug % to not
  then Html.div'_
  else Html.div
    [ Html.styleProp
        [ ("padding", "12px")
        , ("display", "flex")
        , ("flex-direction", "column")
        ]
    ]
    [ Html.text $ "Log : " <> state ^. #debugLog ]


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
              , Html.onMouseenter $ #hover % #overMove .~ True
              , Html.onMouseleave $ #hover % #overMove .~ False
              ]
              ( move & moveCycles & uncycles & toList <&> \cycle ->
                  Html.div
                    [ Html.className "cycle"
                    , Html.onMouseenter $ #hover % #activeCycle ?~ cycle
                    , Html.onMouseleave $ #hover % #activeCycle .~ Nothing
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
    [ debugView state
    , Html.div
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

    where rotate = applyToGemini @Rotation
