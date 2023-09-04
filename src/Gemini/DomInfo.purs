module Gemini.DomInfo
  ( DomInfo
  , loadDomInfo
  , initialDomInfo
  ) where

import Gemini.Prelude
import Data.Map as Map
import FRP.Event as Event

initialDomInfo :: DomInfo
initialDomInfo = { ringRadius: 0.0, ringCenter: mempty }

type DomInfo
  = { ringCenter :: Ring -> Point
    , ringRadius :: Number
    }

loadDomInfo :: Effect DomInfo
loadDomInfo =
  loadDomInfoF
    <#> \domInfo ->
        let
          { ringRadius
          , leftRingCenter
          , centerRingCenter
          , rightRingCenter
          } = domInfo
        in
          { ringRadius
          , ringCenter:
              case _ of
                LeftRing -> leftRingCenter
                CenterRing -> centerRingCenter
                RightRing -> rightRingCenter
          }

foreign import loadDomInfoF :: Effect ForeignDomInfo
type ForeignDomInfo
  = { ringRadius :: Number
    , leftRingCenter :: Point
    , centerRingCenter :: Point
    , rightRingCenter :: Point
    }
