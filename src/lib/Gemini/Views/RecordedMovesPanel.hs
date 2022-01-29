module Gemini.Views.RecordedMovesPanel
  ( recordedMovesPanel
  ) where

import           Relude

import qualified Data.Sequence    as Seq
import           Optics

import           Shpadoinkle
import qualified Shpadoinkle.Html as Html

import           Gemini.Types
import           Utils            (prettyCompactText)


recordedMovesPanel :: Store -> Html m Store
recordedMovesPanel store =
  Html.div
    [ Html.className "saved-moves-panel" ]
    (  (itoListOf (#moves % ifolded) store <&> moveView)
    <> [ Html.div' [ Html.className "scrollbar-pad" ] ]
    )


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
