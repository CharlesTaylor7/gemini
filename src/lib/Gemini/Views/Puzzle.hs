module Gemini.Views.Puzzle
  ( geminiView
  , loadDomInfo
  ) where

import           Relude

import           Data.Angle
import           Data.Cyclic
import           Data.Finitary
import qualified Data.Map                 as Map
import           Data.Permutation
import           Data.Set.Optics
import qualified Data.Text                as Text
import           Data.Traversable         (for)
import           Optics
import           Shpadoinkle              hiding (text)
import qualified Shpadoinkle.Continuation as Continuation
import qualified Shpadoinkle.Html         as Html

import           Gemini.FFI
import           Gemini.Solve
import           Gemini.Types
import           Gemini.UI.Actions
import           Gemini.UI.EventHandlers
import           Gemini.Utils


hiddenLocations :: Store -> Set Location
hiddenLocations store =
  ambiguousLocations
  & map (\(left, right) -> if Just (right ^. #ring) == activeRing store then left else right)
  & fromList
  where
    activeRing :: Store -> Maybe Ring
    activeRing = preview $ to dragAngle % _Just % _1 % #ring


angleOnCircle :: forall n. KnownNat n => Cyclic n -> Angle
angleOnCircle (Cyclic k) = turns ~~ offset
  where
    turns = Turns $ (fromIntegral k) / (fromIntegral $ knownInt @n)
    -- By arbitrary choice, the initial position is at the top of the circle
    offset = Degrees 90


ringClass :: Ring -> Text
ringClass = const "ring " <> \case
  LeftRing   -> "left"
  CenterRing -> "center"
  RightRing  -> "right"


toLetter :: Int -> Char
toLetter i = toEnum $ i + 97

loadDomInfo :: JSM DomInfo
loadDomInfo = do
  ringInfo :: [(Ring, Point)] <- for inhabitants $ \ring -> do
    let selector = ringClass ring & Text.words & Text.intercalate "." & ("." <>)
    elem <- jsCall (jsg ("document" :: Text)) "querySelector" selector
    rect <- jsCall elem "getBoundingClientRect" ()
    width <- rect ! ("width" :: Text) >>= fromJSValUnchecked
    left <- rect ! ("left" :: Text) >>= fromJSValUnchecked
    top <- rect ! ("top" :: Text) >>= fromJSValUnchecked
    let radius = width / 2;
    pure $ (ring, Point (left + radius) (top + radius))

  let getDiameter :: Text -> JSM Double
      getDiameter selector = do
        elem <- jsCall (jsg ("document" :: Text)) "querySelector" selector
        rect <- jsCall elem "getBoundingClientRect" ()
        rect ! ("width" :: Text) >>= fromJSValUnchecked

  ringRadius <- do
    ring <- getDiameter ".ring"
    disk <- getDiameter ".disk"
    pure $ (ring - disk) / 2

  pure $
    DomInfo
      { ringRadius
      , ringCenters = ringInfo & Map.fromList
      }


geminiView :: forall m. Store -> Html m Store
geminiView store =
  Html.div
    [ Html.class'
      [ ("gemini" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    ] $
    concat
      [ [Html.div' [Html.className "background"]]
      , map ringView inhabitants
      -- On load, we capture dom info about the radius of each ring, and their centers.
      -- TODO: listen to window resize to update this info
      , [ invisibleOnLoadView $
            \_ _ -> do
              domInfo <- loadDomInfo
              pure $ Continuation.pur $ #dom .~ domInfo
        ]
      ]
  where
    gemini = store ^. #gemini
    options = store ^. #options
    mobile = options ^. #mobile
    dragged = dragAngle store

    highlighted :: Set Location
    highlighted =
      if options ^. #highlightPairs
      then setOf (to solutionPairs % folded % each) gemini
      else mempty

    activeCycle :: Set Location
    activeCycle = setOf (#hover % _Just % #cycle % _Just % folded) store

    hidden :: Set Location
    hidden = hiddenLocations store

    activeMoveLabels :: Map Location Text
    activeMoveLabels = store
      & itoListOf (#hover % _Just % #move % #moveCycles % each <%> ifolded)
      & map (\((i, j), x) -> (x, show (i + 1) <> Text.singleton (toLetter j)))
      & fromList

    ringView :: Ring -> Html m Store
    ringView ring =
      Html.div [ Html.className $ ringClass ring] $
        disks ring

    disks :: Ring -> [Html m Store]
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")

        draggedAngle :: Angle
        draggedAngle = fromMaybe mempty $ do
           (draggedRing, angle) <- dragged
           guard $ ring == draggedRing ^. #ring
           pure angle

        diskAngle = angleOnCircle position <> draggedAngle

        k = 43
        (x, y) =
          ( k * (1 + cosine diskAngle)
          , k * (1 + sine diskAngle)
          )

        defaultLabel :: Maybe Text
        defaultLabel = (options ^. #showLabels && isn't (#hover % _Just) store) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeMoveLabels ^? ix location

        toLabelSpan label = Html.span [ ("className", "disk-label") ] [ Html.text label ]

        isDraggedDisk :: Location -> Bool
        isDraggedDisk l = Just l == dragged ^? _Just % _1
      in
          Html.div
            ( Html.class'
              [ ("disk", True)
              , (color, True)
              , ("dragging", isDraggedDisk location)
              , ("highlight", highlighted ^. contains location)
              , ("active-cycle", activeCycle ^. contains location)
              , ("hidden", hidden ^. contains location)
              ]
            : Html.styleProp
              [ ("left", show x <> "%")
              , ("top", show y <> "%")
              ]
            : [ ("mousedown", onDragStart location)
              , ("touchstart", onDragStart location)
              ]
            )
            if mobile
            then []
            else [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]


type RawEventHandler m a = RawNode -> RawEvent -> JSM (Continuation m a)

-- | A Hack. Shpadoinkle doesn't have an onMount event like react.
-- So I'm embedding an invisible image with an onload event.
-- The image is already used elsewhere on the page so this should incur minimal overhead.
invisibleOnLoadView :: RawEventHandler m a -> Html m a
invisibleOnLoadView handler =
  Html.img'
    [ ("style", textProp "display: none")
    , ("src", textProp "public/icons/GitHub.png")
    , ("load", listenerProp handler)
    ]
