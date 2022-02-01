module Gemini.Views.App
  ( initialStore
  , rootView
  , Store(..)
  , Options(..)
  , Deployment(..)
  , Env(..)
  ) where

import           Relude

import qualified Data.Sequence                   as Seq
import           Optics
import           System.Random.Stateful          (globalStdGen, uniformM)

import           Shpadoinkle
import qualified Shpadoinkle.Continuation        as Continuation
import qualified Shpadoinkle.Html                as Html
import qualified Shpadoinkle.Keyboard            as Key

import           Gemini.Jsaddle                  (sleep)
import           Gemini.Types
import           Gemini.UI.Actions
import           Gemini.UI.EventHandlers
import           Gemini.Utils
import           Gemini.Views.Puzzle             (geminiView)
import           Gemini.Views.RecordedMovesPanel (recordedMovesPanel)


-- | Initial state of the app
initialStore :: Env -> Store
initialStore env = Store
  { gemini = initialGemini
  , history = Seq.Empty
  , recorded = Seq.Empty
  , moves = Seq.Empty
  , scrambled = False
  , hover = HoverState
    { activeCycle = Nothing
    , overMove = False
    }
  , drag = Nothing
  , options = Options
      { showLabels = False
      , recording = False
      , debug = False
      , isMobile = False
      , confetti = Off
      }
  , env = env
  , dom = DomInfo
    { ringCenters = mempty
    , ringRadius = 0
    }
  }


keyboardMotions :: (Text, Prop m Store)
keyboardMotions =
  Html.onKeydown $ \case
    -- the keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
    Key.Q -> applyRotation $ Rotation LeftRing AntiClockwise
    Key.W -> applyRotation $ Rotation LeftRing Clockwise
    Key.T -> applyRotation $ Rotation CenterRing AntiClockwise
    Key.Y -> applyRotation $ Rotation CenterRing Clockwise
    Key.O -> applyRotation $ Rotation RightRing AntiClockwise
    Key.P -> applyRotation $ Rotation RightRing Clockwise
    _     -> identity


-- | Components
rootView :: MonadJSM m => Store -> Html m Store
rootView store =
  Html.div
    [ Html.class'
      [ ("gemini-app" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    , Html.styleProp
      [ ("justify-content", if isn't (#moves % _Empty) store then "space-between" else "center")
      ]
    -- autofocus so that keyboard events are active on page load
    , Html.autofocus True
    , Html.tabIndex 0
    , keyboardMotions
    -- drag update
    , ("mousemove", onDragUpdate)
    , ("touchmove", onDragUpdate)
    -- drag end
    , ("mouseup", onDragEnd)
    , ("mouseleave", onDragEnd)
    , ("touchend", onDragEnd)
    , ("touchcancel", onDragEnd)
    ]
    [ confettiView store
    , Html.div
        [ Html.className "main-panel"]
        [ header store
        , geminiView store
        , footer store
        ]
    , Html.div
      [ Html.className "right-panel"]
      ( recordedMovesPanel store & toList )
    ]


confettiView :: MonadJSM m => Store -> Html m Store
confettiView store =
  Html.div
    [ Html.class'
      [ ("confetti" :: Text, True)
      , ("fade-in",  store ^. confetti == FadeIn)
      , ("fade-out", store ^. confetti == FadeOut)
      ]
    ]
    [ Html.div
      [ Html.className "stats-box" ]
      [ Html.div
        [ Html.className "stats-header" ]
        [ Html.text "🎉 Solved!!! 🎉" ]
      , Html.div
        [ Html.className "stats" ]
        [ Html.p_
          [ Html.text $
              "Solved in "
            <> (store ^. #history % to length % to show)
            <> " motions"
          ]
        , Html.button
          [ Html.className "action-button"
          , Html.onClickC $
            Continuation.pur (confetti .~ FadeOut)
            `Continuation.before`
            Continuation.merge
              ( Continuation.kleisli $ const $ do
                sleep 1
                pure $ Continuation.pur $ confetti .~ Off
              )
          ]
          [ Html.text "Continue" ]
        ]
      ]
    ]
  where
    confetti :: Lens' Store Confetti
    confetti = #options % #confetti


debugView :: Store -> Maybe (Html m a)
debugView store =
  (store ^. #options % #debug) `orNothing`
  ( Html.div
    [ Html.styleProp
        [ ("padding", "12px")
        , ("display", "flex")
        , ("flex-direction", "column")
        ]
    ]
    ( catMaybes $
      [ flip fmap (store ^. #drag) $
          \drag -> Html.text $ prettyCompactText $ drag
      ]
    )
  )


header :: forall m. MonadJSM m => Store -> Html m Store
header store =
  Html.div
    [ Html.className "header" ]
    [ Html.div
      [ Html.className "control-panel" ]
      ( catMaybes $
        [ debugView store
        , (store ^. #options % #isMobile % to not) `orNothing`
            (
              buttonGroup "options" $
                ( [ (checkBox "Labels" & zoomComponent (#options % #showLabels) store)
                  , (checkBox "Mobile" & zoomComponent (#options % #isMobile) store)
                  ]
                <>
                  if store ^. isProdL
                  then []
                  else
                    [ (checkBox "Debug" & zoomComponent (#options % #debug) store)
                    , (checkBox "Prod" & zoomComponent isProdL store)
                    , (confettiButton & zoomComponent (#options % #confetti) store)
                    ]
                )
            )
        , Just $ buttonGroup "actions" $ catMaybes
            [ Just scrambleButton
            , Just resetButton
            , (store ^. #options % #isMobile % to not) `orNothing` recordButton store
            ]
        ]
      )
    ]
  where
    isProdL :: Lens' Store Bool
    isProdL = #env % #deployment % iso (== Prod) (\bool -> if bool then Prod else Dev)


footer :: Store -> Html m Store
footer store =
  Html.div
    [ Html.className "footer" ]
    [ Html.div
      [ Html.className "links" ]
      [ hyperlink
        "public/icons/GitHub.png"
        ( "https://github.com/CharlesTaylor7/gemini/tree/"
        <> store ^. #env % #commit
        <> "#readme"
        )
        "View Source"
      ]
    ]
  where
    hyperlink :: Text -> Text -> Text -> Html m a
    hyperlink iconSrc linkUrl display =
      Html.a
        [ Html.className "link"
        , Html.href linkUrl
        , Html.target "_blank"
        , Html.rel "noopener noreferrer"
        ]
        [ Html.img [ Html.src iconSrc ] []
        , Html.text display
        ]


buttonGroup :: Text -> [Html m a] -> Html m a
buttonGroup className = Html.div [ Html.class' ["button-group", className] ]


checkBox :: Text -> Bool -> Html m Bool
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


confettiButton :: MonadJSM m => Confetti -> Html m Confetti
confettiButton confetti =
  Html.label
    [ Html.className "checkbox" ]
    [ Html.span
        [ Html.className "checkbox-label" ]
        [ Html.text "Confetti" ]
    , Html.input'
        [ Html.type' "checkbox"
        , Html.checked $ confetti /= Off
        , Html.onCheck $ \checked -> const $ if checked then FadeIn else Off
        ]
    ]

actionButton :: [(Text, Prop m a)] -> [Html m a] -> Html m a
actionButton props = Html.button $ Html.className "action-button" : props


recordButton :: Store -> Html m Store
recordButton store = if store ^. #options % #recording then stopRecordingButton else startRecordingButton
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
        (#history   .~ Seq.Empty) .
        (#recorded  .~ Seq.Empty) .
        (#scrambled .~ False) .
        (#gemini    .~ initialGemini)
    ]
    [ Html.text "Reset" ]


scrambleButton :: forall m. MonadIO m => Html m Store
scrambleButton =
  actionButton
    [ Html.onClickM $ do
        rotations :: [Rotation] <- replicateM 1000 $ uniformM globalStdGen
        let scramble gemini = foldl' (flip applyToGemini) gemini rotations
        pure $
          (#history   .~ Seq.Empty) .
          (#recorded  .~ Seq.Empty) .
          (#gemini    %~ scramble) .
          (#scrambled .~ True)
    ]
    [ Html.text "Scramble" ]
