module Gemini.Views.App
  ( initialStore
  , rootView
  , Store(..)
  , Options(..)
  , Deployment(..)
  , Env(..)
  ) where

import           Relude

import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence           as Seq
import           Optics
import           System.Random.Stateful  (globalStdGen, uniformM)
import           Utils

import           Shpadoinkle
import qualified Shpadoinkle.Html        as Html
import qualified Shpadoinkle.Keyboard    as Key

import           Gemini.Jsaddle
import           Gemini.Types
import           Gemini.UI.Actions
import           Gemini.UI.EventHandlers
import           Gemini.Views.Puzzle     (geminiView)


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
      , confetti = False
      }
  , env = env
  , dom = DomInfo
    { ringCenters = mempty
    , ringRadius = 0
    }
  }


applyRotation :: Rotation -> Store -> Store
applyRotation r = applyMotionToStore $ toMotion r

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
      [ ("gemini-app", True)
      , ("dragging" :: Text, isn't (#drag % _Nothing) store)
      ]
    , Html.tabIndex 0
    , keyboardMotions
    -- on drag
    , ("mousemove", listenerProp onDrag)
    , ("touchmove", listenerProp onDrag)
    -- end drag
    , ("mouseup", listenerProp endDrag)
    , ("mouseleave", listenerProp endDrag)
    , ("touchend", listenerProp endDrag)
    , ("touchcancel", listenerProp endDrag)
    ]
    (
      [ header store
      , Html.div [ Html.className "gemini-row" ] [ geminiView store ]
      , footer store
      ]
    <> (confettiView store & maybe [] pure)
    )


confettiView :: MonadJSM m => Store -> Maybe (Html m Store)
confettiView store =
  (store ^. #options % #confetti ) `orNothing`
  Html.img'
    [ Html.className "confetti"
    , Html.src "https://media.giphy.com/media/5T06ftQWtCMy0XFaaI/giphy.gif"
    , Html.onLoadM $ do
        sleep 3
        pure $ #options % #confetti .~ False
    ]

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


savedMovesPanel :: Store -> Html m Store
savedMovesPanel store =
  Html.div
    [ Html.className "saved-moves-panel" ]
    (  (itoListOf (#moves % ifolded) store <&> moveView)
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


header :: forall m. MonadIO m => Store -> Html m Store
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
                    , (checkBox "Confetti" & zoomComponent (#options % #confetti) store)
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

    where rotate = applyToGemini @Rotation
