module Gemini.Component.App
  ( component
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Deku.Control (text)
import Deku.Listeners as Event
import Deku.Core (Nut)
import Deku.Do as Deku
import Deku.DOM as H
import Deku.Pursx (pursx, (~~))
import Deku.Attributes as Attr
import Deku.Attribute (xdata, (!:=))
import Deku.Hooks (useState)
import Deku.Extra (className)


component :: Nut
component = Deku.do
  H.div
    [ className 
      [ "gemini-app" /\ pure true
      , "dragging" /\ pure false -- isn't (#drag % _Nothing) store
      , "justify-between" /\ pure false 
      , "justify-center" /\ pure true 
      ]
    --, H.Autofocus !:= true
    --, H.Tabindex !:= 0
    ]
    []
{-
    [ confettiView store
    , H.div
        [ H.className "main-panel"]
        [ header store
        , geminiView store
        , debugView store
        , footer store
        ]
    , H.div [ className "right-panel"]
      [ recordedMovesPanel store ]
    ]


confettiView :: MonadJSM m => Store -> H m Store
confettiView store =
  H.div
    [ H.class'
      [ ("confetti" :: Text, True)
      , ("fade-in",  store ^. confetti == FadeIn)
      , ("fade-out", store ^. confetti == FadeOut)
      ]
    ]
    [ H.div [ className "stats-box" ]
      [ H.div [ className "stats-header" ]
        [ H.text "🎉 Solved!!! 🎉" ]
      , H.div [ className "stats" ]
        [ H.p_
          [ H.text $
              "Solved in "
            <> elapsed
            <> " with "
            <> (store ^. #history % to length % to show)
            <> " motions"
          ]
        , H.button
          [ H.className "action-button"
          , H.onClickC $
            Continuation.pur (confetti .~ FadeOut)
            `Continuation.before`
            Continuation.merge
              ( Continuation.kleisli $ const $ do
                sleep 1
                pure $ Continuation.pur $
                  (confetti .~ Off) .
                  (#stats .~ Nothing)
              )
          ]
          [ H.text "Continue" ]
        ]
      ]
    ]
  where
    confetti :: Lens' Store Confetti
    confetti = #options % #confetti

    elapsed :: Text
    elapsed = fromMaybe "cheater" do
      stats <- store ^. #stats
      let Timestamp scrambledAt = stats ^. #scrambledAt
      Timestamp solvedAt <- stats ^. #solvedAt
      pure $ prettyCompactText $ Timestamp (solvedAt - scrambledAt)


debugView :: forall m a. Store -> H m a
debugView store =
  (store ^. #options % #debug) `orEmpty`
  ( H.div
    [ H.styleProp
        [ ("padding", "12px")
        , ("display", "flex")
        , ("flex-direction", "column")
        ]
    ]
    [ paragraph $ prettyText $ store ^. #errors
    , paragraph $ show $ store ^. #gemini % to toSolveStage
    , paragraph $ prettyText $ store ^.. #buffered % each
    -- paragraph $ prettyCompactText $ store ^.. #gemini % to solutionPairs % folded
    ]
  )
  where
    paragraph :: forall m a. Text -> H m a
    paragraph = H.p_ . pure . Html.text


header :: forall m. MonadJSM m => Store -> H m Store
header store =
  H.div [ className "header" ]
  [ H.div [ className "control-panel" ]
    [ htmlWhen (store ^. #options % #mobile % to not) $
      buttonGroup "options" $
      [ checkBox "Labels" & option #showLabels
      , checkBox "Keyboard controls?" & option #showKeyboardShortcuts
      , checkBox "Bot controls?" & option #showBotControls
      ]
      <>
      if store ^. #env % #deployment % to (== Prod)
      then []
      else
        [ --confettiButton & option #confetti
          checkBox "Debug" & option #debug
        , checkBox "Highlight pairs" & option #highlightPairs
        ]
    , buttonGroup "actions"
      [ scrambleButton
      --, (store ^. #options % #mobile % to not) `orEmpty` recordButton store
      , nextBotMoveButton store
      , undoButton store
      , H.text "ticks per"
      , numberInput & zoomComponent (#animation % #ticksPerRotation) store
      , H.text "refresh rate"
      , refreshRateInput $ store ^. #animation % #refreshRate
      ]
    ]
  ]
  where
    option :: Optic' A_Lens ix Options s -> (s -> H m s) -> Html m Store
    option optic = zoomComponent (#options % optic) store


footer :: Store -> H m Store
footer store =
  H.div [ className "footer" ]
  [ H.div [ className "links" ]
    [ hyperlink
      "public/icons/GitHub.png"
      ( "https://github.com/CharlesTaylor7/gemini/tree/"
      <> store ^. #env % #commit
      <> "#readme"
      )
      "View Source"
    ]
  , htmlWhen (store ^. #options % #showKeyboardShortcuts) $
      H.div [ className "explain-controls" ]
      [ H.div_ $ pure $ Html.text "Q: Rotate left disk counter clockwise"
      , H.div_ $ pure $ Html.text "W: Rotate left disk clockwise"
      , H.div_ $ pure $ Html.text "T: Rotate center disk counter clockwise"
      , H.div_ $ pure $ Html.text "Y: Rotate center disk clockwise"
      , H.div_ $ pure $ Html.text "O: Rotate right disk counter clockwise"
      , H.div_ $ pure $ Html.text "P: Rotate right disk clockwise"
      ]
  ]
  where
    hyperlink :: Text -> Text -> Text -> H m a
    hyperlink iconSrc linkUrl display =
      H.a
        [ H.className "link"
        , H.href linkUrl
        , H.target "_blank"
        , H.rel "noopener noreferrer"
        ]
        [ H.img [ Html.src iconSrc ] []
        , H.text display
        ]


buttonGroup :: Text -> [H m a] -> Html m a
buttonGroup className = H.div [ Html.class' ["button-group", className] ]


checkBox :: Text -> Bool -> H m Bool
checkBox label checked =
  H.label
    [ H.className "checkbox" ]
    [ H.span
        [ H.className "checkbox-label" ]
        [ H.text label ]
    , H.input'
        [ H.type' "checkbox"
        , H.checked checked
        , H.onCheck const
        ]
    ]


actionButton :: [(Text, Prop m a)] -> [H m a] -> Html m a
actionButton props = H.button $ className "action-button" : props


recordButton :: Store -> H m Store
recordButton store =
  if store ^. #options % #recording
  then stopRecordingButton
  else startRecordingButton
  where
    startRecordingButton :: H m Store
    startRecordingButton =
      actionButton
        [ H.onClick $ #options % #recording .~ True ]
        [ H.text $ "Start Recording" ]


    stopRecordingButton :: H m Store
    stopRecordingButton =
      actionButton
        [ H.onClick $ stopRecording ]
        [ H.text $ "Stop Recording" ]



undoButton :: Monad m => Store -> H m Store
undoButton _store =
  actionButton
    [ H.onClickC $ do
        Actions.run $ do
          frame <- use $ #animation % #frame
          when (is #_Nothing frame) $ do
            history <- use #history
            case history of
              Seq.Empty -> pure ()
              h :<| rest -> do
                (#history .= rest)
                (#gemini %= applyToGemini (h & #amount %~ invert))
    ]
    [ H.text "Undo" ]


numberInput :: Int -> H m Int
numberInput initial =
  H.input'
    [ H.className "number-input"
    , H.type' "number"
    , H.onInput \text old ->
        case Read.decimal text of
          Right (new, _) -> new
          Left _         -> old
    , H.value (show initial)
    ]


refreshRateInput :: MonadJSM m => Int -> H m Store
refreshRateInput initial =
  H.input'
    [ H.className "number-input"
    , H.type' "number"
    , H.value (show initial)
    , H.onInputC \text ->
        Actions.run $
          case Read.decimal text of
            Right (num, _) -> Actions.updateRefreshRate num
            Left _         -> pure ()
    ]


nextBotMoveButton :: MonadJSM m => Store -> H m Store
nextBotMoveButton store =
  actionButton
    [ H.onClickC $ do
        Actions.run $ do
          Actions.updateRefreshRate $ store ^. #animation % #refreshRate
          let move = Solve.nextMove $ store ^. #gemini
          Actions.applyBotMove move
    ]
    [ H.text "Next" ]

scrambleButton :: forall m. MonadJSM m => H m Store
scrambleButton =
  actionButton
    [ H.onClickM $ do
        rotations :: [Rotation] <- replicateM 1000 $ uniformM globalStdGen
        let scramble gemini = foldl' (flip applyToGemini) gemini rotations
        scrambledAt <- dateNow
        pure $
          (#history  .~ Seq.Empty) .
          (#recorded .~ Seq.Empty) .
          (#gemini   %~ scramble) .
          (#stats    ?~ Stats { scrambledAt, solvedAt = Nothing })
    ]
    [ H.text "Scramble" ]

-}
