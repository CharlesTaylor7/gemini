module Gemini.Html
  ( geminiView
  ) where

import           Relude

import           Data.Cyclic
import           Data.Finitary
import           Data.Permutation
import           Data.Set.Optics
import qualified Data.Text                   as Text
import           Language.Javascript.JSaddle (JSVal, MakeArgs, ToJSVal (..), fromJSValUnchecked, instanceOf, jsg, (!!),
                                              (!), (#))
import           Optics                      hiding ((#))
import           Shpadoinkle                 hiding (text)
import qualified Shpadoinkle.Html            as Html

import           Gemini.Types
import           Gemini.UI.Actions
import           Gemini.UI.EventHandlers
import           Utils


sine = sin . toRadians
cosine = cos . toRadians


toRadians :: Double -> Double
toRadians th = (th * pi) / 180.0


toDegrees :: Double -> Double
toDegrees th = (th * 180) / pi


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

activeRing :: Store -> Maybe Ring
activeRing store = inhabitants & filter (isActive store) & preview (ix 0)

isActive :: Store -> Ring -> Bool
isActive store r = fromMaybe False $ do
  drag <- store ^. #drag
  pure $ drag ^. #location % #ring == r


angle :: Point -> Double
angle (Point x y) = atan2 y x & toDegrees


jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args

jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text


geminiView :: forall m. Applicative m => Store -> Html m Store
geminiView store =
  Html.div
    [ Html.class'
      [ ("gemini" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    ]
    (  map ringView inhabitants
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
          [ ("ring", True)
          , (ringClass ring, True)
          , ("dragging", isActive store ring)
          ]
        ]
        ( disks ring )

    disks :: Ring -> [Html m Store]
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")

        draggedAngle :: Double
        draggedAngle = fromMaybe 0 $ do
           (draggedRing, angle) <- dragged
           guard $ ring == draggedRing
           pure angle

        diskAngle = fromIntegral (unCyclic position) * 20.0 - 90.0 + draggedAngle
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
        isDraggedDisk l = Just l == store ^? #drag % _Just % #location

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
