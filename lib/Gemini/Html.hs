module Gemini.Html
  ( geminiView
  ) where

import           Relude

import           Data.Angle
import           Data.Cyclic
import           Data.Finitary
import qualified Data.Map                    as Map
import           Data.Permutation
import           Data.Set.Optics
import qualified Data.Text                   as Text
import           Data.Traversable            (for)
import           Language.Javascript.JSaddle (JSVal, MakeArgs, ToJSVal (..), fromJSValUnchecked, instanceOf, jsg, (!!),
                                              (!), (#))
import           Optics                      hiding ((#))
import           Shpadoinkle                 hiding (text)
import qualified Shpadoinkle.Continuation    as Continuation
import qualified Shpadoinkle.Html            as Html

import           Gemini.Types
import           Gemini.UI.Actions
import           Gemini.UI.EventHandlers
import           Utils


draggedOver :: Store -> Set Location
draggedOver store = setOf (folded % to sibling % _Just) $ activeLocations store

cycleOver :: Store -> Set Location
cycleOver store = setOf (#hover % #activeCycle % _Just % folded % to sibling % _Just) $ store

hiddenLocations :: Store -> Set Location
hiddenLocations = hidden
  where
    hidden = draggedOver <> cycleOver
    noncanonical store =
      case hidden store of
        Empty -> inhabitants & filter (not . isCanonical) & fromList
        set   -> set

activeLocations :: Store -> [Location]
activeLocations store = activeRing store & concatMap (\ring -> map (Location ring) inhabitants)
  where
    activeRing :: Store -> Maybe Ring
    activeRing = preview $ to dragAngle % _Just % _1 % #ring



jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args

jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text

angleOnCircle :: forall n. KnownNat n => Cyclic n -> Angle
angleOnCircle (Cyclic k) = Turns $ (fromIntegral k) / (fromIntegral $ knownInt @n)

geminiView :: forall m. Applicative m => Store -> Html m Store
geminiView store =
  Html.div
    [ Html.class'
      [ ("gemini" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    ]
    (  map ringView inhabitants
    -- On load, we capture dom info about the radius of each ring, and their centers.
    -- TODO: listen to window resize to update this info
    <> [ invisibleOnLoadView $ \_ _ -> do
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

          let domInfo = DomInfo
                { ringRadius
                , ringCenters = ringInfo & Map.fromList
                }

          pure $ Continuation.pur $ #dom .~ domInfo
       ]
    )
  where
    gemini = store ^. #gemini
    options = store ^. #options
    isMobile = options ^. #isMobile
    dragged = dragAngle store

    activeCycleMap :: Map Location Int
    activeCycleMap = store
      & itoListOf (#hover % #activeCycle % non (Cycle Empty) % ifolded)
      & map (\(i, x) -> (x, i + 1))
      & fromList

    ringView :: Ring -> Html m Store
    ringView ring =
      Html.div
        [ Html.class'
          [ (ringClass ring, True)
          ]
        ]
        ( disks ring
        )

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

        diskAngle = (angleOnCircle position ~~ (Degrees 90)) <> draggedAngle

        k = 43
        (x, y) =
          ( k * (1 + cosine diskAngle)
          , k * (1 + sine diskAngle)
          )

        defaultLabel :: Maybe Text
        defaultLabel = (options ^. #showLabels && store ^. #hover % #overMove % to not) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeCycleMap ^? ix location <&> show

        toLabelSpan label = Html.div [ ("className", "disk-label") ] [ Html.text label ]

        isDraggedDisk :: Location -> Bool
        isDraggedDisk l = Just l == dragged ^? _Just % _1

        onDragStart = listenerProp $ startDrag location
      in
          Html.div
            ( Html.class'
              [ ("disk", True)
              , (color, True)
              , ("dragging", isDraggedDisk location)
              , ("hidden", hiddenLocations store ^. contains location)
              ]
            : Html.styleProp
              [ ("left", show x <> "%")
              , ("top", show y <> "%")
              ]
            : [ ("mousedown" , onDragStart)
              , ("touchstart" , onDragStart)
              ]
            )
            if isMobile
            then []
            else [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]


-- | A cheap trick. Shpadoinkle doesn't have an onmount event like react.
-- So I'm embedding an invisible image with an onload event.
-- The image is already used elsewhere on the page so this should incur minimal overhead.
invisibleOnLoadView :: RawEventHandler m a -> Html m a
invisibleOnLoadView handler =
  Html.img'
    [ ("style", textProp "display: none")
    , ("src", textProp "public/icons/GitHub.png")
    , ("load", listenerProp handler)
    ]
