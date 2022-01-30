module Gemini.Views.RecordedMovesPanel
  ( recordedMovesPanel
  ) where

import           Relude

import qualified Data.Sequence     as Seq
import           Optics

import           Shpadoinkle
import qualified Shpadoinkle.Html  as Html

import           Gemini.Types
import           Gemini.UI.Actions
import           Gemini.Utils      (orNothing, prettyCompactText)


recordedMovesPanel :: Store -> Maybe (Html m Store)
recordedMovesPanel store =
  (store ^. #options % #isMobile % to not) `orNothing`
  Html.div
    [ Html.className "saved-moves-panel" ]
    ( (bufferView store & toList)
    <> (itoListOf (#moves % ifolded) store <&> moveView )
    )

bufferView :: Store -> Maybe (Html m Store)
bufferView store =
  (store ^. #options % #recording) `orNothing`
  Html.div
    [ Html.className "recorded-buffer" ]
    [ Html.text $ prettyCompactText $ store ^.. #recorded % folded ]


moveView :: (Int, Move) -> Html m Store
moveView (i, move) =
  Html.div
    [ Html.className "move" ]
    [ Html.div
      [ Html.className "move-description"
      , Html.onClick $ applyMove move
      ]
      [ Html.div
          [ Html.className "motions" ]
          [ Html.text $ (prettyCompactText $ move ^.. #motions % folded ) <> ":" ]
      , Html.div
          [ Html.className "cycles"
          , Html.onMouseenter $ #hover % #overMove .~ True
          , Html.onMouseleave $ #hover % #overMove .~ False
          ]
          ( move & moveCycles & unCycles & toList <&> \cycle ->
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
