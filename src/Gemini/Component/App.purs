module Gemini.Views.App
  ( component
  ) where

component :: Int
component = 2

{-
-- | Components
rootView :: MonadJSM m => Store -> Html m Store
rootView store =
  Html.div
    [ Html.class'
      [ ("gemini-app" :: Text, True)
      , ("dragging", False)
      ]
    , Html.styleProp
      [ ("justify-content"
        , if store ^. #options % #recording || isn't (#moves % _Empty) store
          then "space-between"
          else "center"
        )
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
        , debugView store
        , footer store
        ]
    , Html.div [ Html.className "right-panel"]
      [ recordedMovesPanel store ]
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
    [ Html.div [ Html.className "stats-box" ]
      [ Html.div [ Html.className "stats-header" ]
        [ Html.text "🎉 Solved!!! 🎉" ]
      , Html.div [ Html.className "stats" ]
        [ Html.p_
          [ Html.text $
              "Solved in "
            <> elapsed
            <> " with "
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
                pure $ Continuation.pur $
                  (confetti .~ Off) .
                  (#stats .~ Nothing)
              )
          ]
          [ Html.text "Continue" ]
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


debugView :: forall m a. Store -> Html m a
debugView store =
  (store ^. #options % #debug) `orEmpty`
  ( Html.div
    [ Html.styleProp
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
    paragraph :: forall m a. Text -> Html m a
    paragraph = Html.p_ . pure . Html.text


header :: forall m. MonadJSM m => Store -> Html m Store
header store =
  Html.div [ Html.className "header" ]
  [ Html.div [ Html.className "control-panel" ]
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
      , Html.text "ticks per"
      , numberInput & zoomComponent (#animation % #ticksPerRotation) store
      , Html.text "refresh rate"
      , refreshRateInput $ store ^. #animation % #refreshRate
      ]
    ]
  ]
  where
    option :: Optic' A_Lens ix Options s -> (s -> Html m s) -> Html m Store
    option optic = zoomComponent (#options % optic) store


footer :: Store -> Html m Store
footer store =
  Html.div [ Html.className "footer" ]
  [ Html.div [ Html.className "links" ]
    [ hyperlink
      "public/icons/GitHub.png"
      ( "https://github.com/CharlesTaylor7/gemini/tree/"
      <> store ^. #env % #commit
      <> "#readme"
      )
      "View Source"
    ]
  , htmlWhen (store ^. #options % #showKeyboardShortcuts) $
      Html.div [ Html.className "explain-controls" ]
      [ Html.div_ $ pure $ Html.text "Q: Rotate left disk counter clockwise"
      , Html.div_ $ pure $ Html.text "W: Rotate left disk clockwise"
      , Html.div_ $ pure $ Html.text "T: Rotate center disk counter clockwise"
      , Html.div_ $ pure $ Html.text "Y: Rotate center disk clockwise"
      , Html.div_ $ pure $ Html.text "O: Rotate right disk counter clockwise"
      , Html.div_ $ pure $ Html.text "P: Rotate right disk clockwise"
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


actionButton :: [(Text, Prop m a)] -> [Html m a] -> Html m a
actionButton props = Html.button $ Html.className "action-button" : props


recordButton :: Store -> Html m Store
recordButton store =
  if store ^. #options % #recording
  then stopRecordingButton
  else startRecordingButton
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



undoButton :: Monad m => Store -> Html m Store
undoButton _store =
  actionButton
    [ Html.onClickC $ do
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
    [ Html.text "Undo" ]


numberInput :: Int -> Html m Int
numberInput initial =
  Html.input'
    [ Html.className "number-input"
    , Html.type' "number"
    , Html.onInput \text old ->
        case Read.decimal text of
          Right (new, _) -> new
          Left _         -> old
    , Html.value (show initial)
    ]


refreshRateInput :: MonadJSM m => Int -> Html m Store
refreshRateInput initial =
  Html.input'
    [ Html.className "number-input"
    , Html.type' "number"
    , Html.value (show initial)
    , Html.onInputC \text ->
        Actions.run $
          case Read.decimal text of
            Right (num, _) -> Actions.updateRefreshRate num
            Left _         -> pure ()
    ]


nextBotMoveButton :: MonadJSM m => Store -> Html m Store
nextBotMoveButton store =
  actionButton
    [ Html.onClickC $ do
        Actions.run $ do
          Actions.updateRefreshRate $ store ^. #animation % #refreshRate
          let move = Solve.nextMove $ store ^. #gemini
          Actions.applyBotMove move
    ]
    [ Html.text "Next" ]

scrambleButton :: forall m. MonadJSM m => Html m Store
scrambleButton =
  actionButton
    [ Html.onClickM $ do
        rotations :: [Rotation] <- replicateM 1000 $ uniformM globalStdGen
        let scramble gemini = foldl' (flip applyToGemini) gemini rotations
        scrambledAt <- dateNow
        pure $
          (#history  .~ Seq.Empty) .
          (#recorded .~ Seq.Empty) .
          (#gemini   %~ scramble) .
          (#stats    ?~ Stats { scrambledAt, solvedAt = Nothing })
    ]
    [ Html.text "Scramble" ]

-}
