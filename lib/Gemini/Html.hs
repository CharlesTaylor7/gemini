module Gemini.Html
  ( geminiHtmlView
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



-- dimensions
ringR = 150.0
diskR = (ringR / 7.0)
ringD = 2 * ringR
diskD = 2 * diskR

-- math utils
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
  pure $ drag ^. #ring == r



angle :: Point -> Double
angle (Point x y) = atan2 y x & toDegrees


jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args

jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text


geminiHtmlView :: forall m. Applicative m => Store -> Html m Store
geminiHtmlView store =
  Html.div
    [ Html.class'
      [ ("gemini" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    ]
    (  map ring inhabitants
    )
  where
    gemini = store ^. #gemini
    options = store ^. #options
    isMobile = options ^. #isMobile

    activeCycleMap :: Map Location Int
    activeCycleMap = store
      & itoListOf (#hover % #activeCycle % non (Cycle Empty) % ifolded)
      & map (\(i, x) -> (x, i + 1))
      & fromList
    ring :: Ring -> Html m Store
    ring r =
      Html.div
        [ Html.class'
          [ ("ring", True)
          , ("ring-" <> prettyCompactText r, True)
          , ("dragging", isActive store r)
          ]

        , Html.styleProp
            [ ("width", show ringD <> "px")
            , ("height", show ringD <> "px")
            ]
        ]
        ( disks r )

    dragAngle :: Ring -> Double
    dragAngle r = fromMaybe 0 $ do
      drag <- store ^. #drag
      guard $ drag ^. #ring == r
      pure $ drag ^. #currentAngle

    disks :: Ring -> [Html m Store]
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")
        diskAngle = fromIntegral (unCyclic position) * 20.0 - 90.0 + dragAngle ring
        r = ringR - diskR
        (x, y) =
          ( r * (1 + cosine diskAngle)
          , r * (1 + sine diskAngle)
          )

        defaultLabel :: Maybe Text
        defaultLabel = (options ^. #showLabels && store ^. #hover % #overMove % to not) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeCycleMap ^? ix location <&> show

        toLabelSpan label = Html.div [ ("className", "disk-label") ] [ Html.text label ]
      in
          Html.div
            ( Html.class'
              [ ("disk", True)
              , (color, True)
              , ("drag-disabled", isIntersection location)
              , ("hidden", hiddenLocations store ^. contains location)
              ]
            : Html.styleProp
              [ ("width", show diskD <> "px")
              , ("height", show diskD <> "px")
              , ("line-height", show diskD <> "px")
              , ("left", show x <> "px")
              , ("top", show y <> "px")
              ]
            : if isIntersection location
              then []
              else [ ("mousedown" , listenerProp $ startDrag ring)
                   , ("touchstart" , listenerProp $ startDrag ring)
                   ]
            )
            if isMobile
            then []
            else [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]
