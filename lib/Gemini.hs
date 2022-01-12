{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude

import qualified Data.Text                 as Text
import           Optics                    hiding ((#))
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty

import           Svg                       (SvgElement)
import qualified Svg

import           Shpadoinkle
import qualified Shpadoinkle.Html          as Html
import qualified Shpadoinkle.Keyboard      as Key
import           Shpadoinkle.Lens          (generalize)

import           Gemini.Html
import           Gemini.Types


-- | Store operations
data Store = Store
  { gemini          :: !Gemini
  , history         :: ![Rotation]
  , diskLabelOption :: !DiskLabelOption
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


applyRotation :: Rotation -> Store -> Store
applyRotation r = over #gemini (rotate r) . over #history (r :)


initialState :: Store
initialState = Store
  { gemini = solvedGemini
  , history = []
  , diskLabelOption = HideLabels
  }


-- | Components
rootView :: MonadIO m => Store -> Html m Store
rootView state =
  Html.div
    [ Html.className "gemini-app"
    , Html.onKeydown $ \key ->
      case key of
        -- the keyboard shortcuts are based on the top row of keys in the rightmost positions:
        -- T, Y, U, I, O, P
        Key.T -> applyRotation $ Rotation LeftRing Clockwise
        Key.Y -> applyRotation $ Rotation LeftRing AntiClockwise
        Key.U -> applyRotation $ Rotation CenterRing Clockwise
        Key.I -> applyRotation $ Rotation CenterRing AntiClockwise
        Key.O -> applyRotation $ Rotation RightRing Clockwise
        Key.P -> applyRotation $ Rotation RightRing AntiClockwise
        _     -> identity

    ]
    [ buttonsRow state
    , geminiHtmlView (state ^. #diskLabelOption) (state ^. #gemini)
    , debugView state
    ]


buttonsRow :: Functor m => Store -> Html m Store
buttonsRow state =
  Html.div
    [ Html.className "motion-buttons-row"
    ]
    [ Html.div
        [ Html.className "motion-buttons-group"
        ]
        ( (diskLabelOptionToggle (state ^. #diskLabelOption) & generalizeOptic #diskLabelOption)
        : ( flip map rotations $ \rotation ->
              Html.button
                [ Html.className "motion-button"
                , Html.onClick $ applyRotation rotation
                ]
                [ Html.text $ prettyCompactText rotation
                ]
          )
        )
    ]
  where
    rotations =
      [ Rotation LeftRing Clockwise
      , Rotation LeftRing AntiClockwise
      , Rotation CenterRing Clockwise
      , Rotation CenterRing AntiClockwise
      , Rotation RightRing Clockwise
      , Rotation RightRing AntiClockwise
      ]


diskLabelOptionToggle :: DiskLabelOption -> Html m DiskLabelOption
diskLabelOptionToggle option =
  Html.button
    [ Html.onClick opposite]
    [ Html.text $ show $ opposite option
    ]
    where
      opposite = \case
        ShowLabels -> HideLabels
        HideLabels -> ShowLabels


debugView :: Store -> Html m a
debugView state =
  Html.div
    [ Html.className "gemini-debug"]
    [ Html.text $ prettyCompactText (state ^. #history % to (take 10))
    ]



-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


generalizeOptic :: (Functor m, Continuous f, Is k A_Lens) => Optic' k is s a -> (f m a -> f m s)
generalizeOptic arg = generalize $ toLensVL arg
