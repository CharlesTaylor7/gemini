module Gemini.DomInfo 
  ( DomInfo
  , loadDomInfo
  ) where

import Gemini.Prelude
import Data.Map as Map

type DomInfo = 
  { ringCenter :: Ring -> Point
  , ringRadius :: Number
  }


loadDomInfo :: Effect DomInfo
loadDomInfo = 
  loadDomInfoF <#> \domInfo ->
    let 
      { ringRadius
      , leftRingCenter
      , centerRingCenter
      , rightRingCenter 
      } = domInfo
    in
      { ringRadius
      , ringCenter: case _ of
          LeftRing -> leftRingCenter
          CenterRing -> centerRingCenter
          RightRing -> rightRingCenter
      }


foreign import loadDomInfoF :: Effect ForeignDomInfo
type ForeignDomInfo = 
  { ringRadius :: Number
  , leftRingCenter :: Point
  , centerRingCenter :: Point
  , rightRingCenter :: Point
  }
