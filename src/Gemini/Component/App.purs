module Gemini.Component.App
  ( component
  ) where

import Gemini.Prelude
import Gemini.Store (Store, useStore)
import Gemini.Component.Puzzle as Puzzle
import Gemini.DomInfo (initialDomInfo, loadDomInfo)
import Gemini.Component.App.Actions (keyboardEvents, scramble)
import Gemini.Component.Puzzle.Actions (onDragEnd, onDragUpdate)
import Gemini.Component.Puzzle.Mobile as Mobile
import Data.Gemini as Gemini
import Deku.Do as Deku
import Deku.DOM as D
import Resize as Resize
import Gemini.Store as Store
import ClassName as Class
import Effect.Console as Console

component :: Nut
component = Deku.do
  gemini <- useStore Gemini.initialGemini
  drag <- useStore (Nothing :: _ Drag)
  pushConfetti /\ confetti <- useState'
  useAff confetti
    $ case _ of
        FadeIn -> do
          liftEffect $ Console.log "fade in"
          {-
          delay $ Milliseconds 1000.0
          liftEffect $ pushConfetti FadeOut
          -}

        FadeOut -> do
          liftEffect $ Console.log "fade out"
          delay $ Milliseconds 1000.0
          liftEffect $ pushConfetti Off

        Off -> do
          liftEffect $ Console.log "off"

  let resize = Resize.observe
  domInfo <- useRef initialDomInfo $ bindToEffect resize.event $ const loadDomInfo
  let props = { gemini, drag, domInfo, pushConfetti }
  if isTouchDevice then
    ( pursx ::
        _ """
          <div class="w-full h-full fixed" ~attrs~>
            ~puzzle~
          </div>
        """
    )
      ~~
        { attrs:
            (D.Self !:= \e -> resize.listen e)
              <|> (D.OnTouchmove !:= touch (onDragUpdate props))
              <|> (D.OnTouchend !:= touch (onDragEnd props))
              <|> (D.OnTouchcancel !:= touch (onDragEnd props))
        , puzzle: Mobile.component props
        }
  else
    ( pursx ::
        _ """
          <div ~attrs~>
            <div ~confettiAttrs~ >
            </div>
            <div class="mt-12 flex flex-col gap-12 items-center">
              ~header~
              ~puzzle~
              ~footer~
            </div>
          </div>
        """
    )
      ~~
        { header: header gemini
        , confettiAttrs:
            Class.name
              [ pure "confetti"
              , "fade-in" # Class.when (confetti <#> eq FadeIn)
              , "fade-out" # Class.when (confetti <#> eq FadeOut)
              ]
        -- TODO: use oneOf, or whatever is more efficient
        , attrs:
            Class.name
              [ pure "fixed w-full h-full flex justify-center"
              , "cursor-grabbing" # Class.when (Store.subscribe drag <#> isJust)
              ]
              <|> autoFocus
              <|> tabIndex (pure 0)
              <|> (D.Self !:= \e -> resize.listen e)
              <|> (D.OnKeydown !:= keyboard (keyboardEvents props))
              <|> (D.OnMousemove !:= pointer (onDragUpdate props))
              <|> (D.OnMouseup !:= pointer (onDragEnd props))
              <|> (D.OnMouseleave !:= pointer (onDragEnd props))
        , puzzle: Puzzle.component props
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
