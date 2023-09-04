module Gemini.Component.App
  ( component
  ) where

import Gemini.Prelude
import Gemini.Store (Store, useStore)
import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle
import Gemini.DomInfo (DomInfo, initialDomInfo, loadDomInfo)
import Gemini.Component.App.Actions
import Gemini.Component.Puzzle.Actions
import Gemini.Component.Puzzle.Mobile as Mobile
import Data.Gemini as Gemini
import Deku.Do as Deku
import Deku.DOM as D
import Resize as Resize
import Gemini.Store as Store
import ClassName as Class

component :: Nut
component = Deku.do
  gemini <- useStore Gemini.initialGemini
  drag <- useStore (Nothing :: _ Drag)
  let resize = Resize.observe
  let domInfoEvent = resize.event `bindToEffect` const loadDomInfo
  domInfo <- useRef initialDomInfo domInfoEvent
  let props = { gemini, drag, domInfo }
  if isTouchDevice
  then Mobile.component props
  else
    ( pursx ::
        _ """
      <div ~attrs~>
        <div class="flex flex-col gap-12 items-center">
          ~header~
          ~puzzle~
          ~footer~
        </div>
      </div>
    """
    )
    ~~ let
        dragEnd = pointer $ onDragEnd props
      in
        { header: header gemini
        -- TODO: use oneOf, or whatever is more efficient
        , attrs:
            Class.name
              [ pure "mt-12 w-full h-full flex justify-center fixed"
              , "cursor-grabbing" # Class.when (Store.subscribe drag <#> isJust)
              ]
              <|> (D.Self !:= \e -> resize.listen e)
              <|> autoFocus
              <|> tabIndex (pure 0)
              <|> (D.OnKeydown !:= keyboard (keyboardEvents gemini))
              <|> (D.OnPointermove !:= pointer (onDragUpdate props))
              <|> (D.OnPointerup !:= dragEnd)
              <|> (D.OnPointerleave !:= dragEnd)
              <|> (D.OnPointercancel !:= dragEnd)
        , puzzle:
            Puzzle.component props
        , footer
        }

header :: Store Gemini -> Nut
header store =
  ( pursx ::
      _ """
    <div ~headerAttrs~>
      <button class="action-button" ~buttonAttrs~>
        Scramble
      </button>
    </div>
  """
  )
    ~~
      { headerAttrs:
          Class.name
            [ pure "flex justify-center"
            , "hidden" # Class.when (pure isTouchDevice)
            ]
      , buttonAttrs: 
          D.OnClick !:= scramble store
      }

footer :: Nut
footer =
  ( pursx ::
      _ """
      <div ~footer~>
        <div class="text-2xl">
          <div>Q: Rotate left disk counter clockwise</div>
          <div>W: Rotate left disk clockwise</div>
          <div>T: Rotate center disk counter clockwise</div>
          <div>Y: Rotate center disk clockwise</div>
          <div>O: Rotate right disk counter clockwise</div>
          <div>P: Rotate right disk clockwise</div>
        </div>
        <div class="fixed bottom-0 left-0">
          ~viewSource~
        </div>
      </div>
    """
  )
    ~~
      { footer: Class.name [ "hidden" # Class.when (pure isTouchDevice) ]
      , viewSource:
          hyperlink
            "./github.png"
            "https://github.com/CharlesTaylor7/gemini"
            "View Source"
      }

hyperlink :: String -> String -> String -> Nut
hyperlink iconSrc linkUrl display =
  ( pursx ::
      _ """
    <a target="_blank" rel="noopener noreferrer" ~linkAttrs~>
        <img ~imgAttrs~ />
        <span ~labelAttrs~>~label~</span>
    </a>
  """
  )
    ~~
      { linkAttrs:
          klass_ "flex items-center p-2 underline decoration-sky-500/30"
            <|> (D.Href !:= linkUrl)
      , imgAttrs:
          klass_ "h-[20px] m-2"
            <|> (D.Src !:= iconSrc)
      , labelAttrs:
          klass_ "decoration-sky-500/30"
      , label:
          text_ display
      }
