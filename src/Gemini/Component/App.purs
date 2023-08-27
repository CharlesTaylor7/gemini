module Gemini.Component.App
  ( component
  ) where

import Gemini.Prelude
import Control.Alt ((<|>))
import Data.Gemini as Gemini
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Resize as Resize
import Deku.Control (text, text_)
import Deku.Listeners as Listener
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, href_)
import Deku.Attribute (xdata, (!:=), unsafeAttribute, AttributeValue(..))
import Deku.Attribute as Attr
import Deku.Extra (className, autoFocus, tabIndex)
import Deku.Hooks (useEffect, useRef)
import Gemini.Store (Store, useStore)
import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle
import Gemini.DomInfo (DomInfo)
import Gemini.Component.App.Actions
import Gemini.Component.Puzzle.Actions
import Gemini.DomInfo

component :: Nut
component = Deku.do
  gemini <- useStore Gemini.initialGemini
  drag <- useStore (Nothing :: _ Drag)
  let resize = Resize.observe
  domInfo <- useRef initialDomInfo (resize.event `bindToEffect` const loadDomInfo)
  ( pursx ::
      _ """
    <div class="gemini-app" ~attrs~>
      <div class="main-panel">
        ~header~
        ~puzzle~
        ~footer~
      </div>
      <div class="right-panel" data-todo="recorded-moves">
      </div>
    </div>
  """
  )
    ~~ let
        dragEnd = Attr.cb $ onDragEnd { gemini, drag }
      in
        { header: header gemini
        , attrs:
            (D.Self !:= \e -> resize.listen e)
              <|> autoFocus
              <|> tabIndex (pure 0)
              <|> Listener.keyDown_ (keyboardEvents gemini)
              <|> (D.OnPointermove !:= Attr.cb (onDragUpdate drag))
              <|> (D.OnPointerup !:= dragEnd)
              <|> (D.OnPointerleave !:= dragEnd)
              <|> (D.OnPointercancel !:= dragEnd)
        , puzzle: Puzzle.component { gemini, drag }
        , footer
        }

header :: Store Gemini -> Nut
header store =
  D.div [ klass_ "header" ]
    [ D.div [ klass_ "control-panel" ]
        [ D.button
            [ klass_ "action-button"
            , Listener.click_ $ scramble store
            ]
            [ text_ "Scramble" ]
        {-
    , D.button
      [ klass_ "action-button" ]
      [ text_ "Undo" ]
    -}
        ]
    ]

footer :: Nut
footer =
  D.div []
    [ D.div [ klass_ "text-2xl" ]
        [ D.div__ "Q: Rotate left disk counter clockwise"
        , D.div__ "W: Rotate left disk clockwise"
        , D.div__ "T: Rotate center disk counter clockwise"
        , D.div__ "Y: Rotate center disk clockwise"
        , D.div__ "O: Rotate right disk counter clockwise"
        , D.div__ "P: Rotate right disk clockwise"
        ]
    , D.div [ klass_ "fixed bottom-0 left-0" ]
        [ hyperlink
            "./github.png"
            "https://github.com/CharlesTaylor7/gemini"
            "View Source"
        ]
    ]

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
            <|> href_ linkUrl
      , imgAttrs:
          klass_ "h-[20px] m-2"
            <|> D.Src
            !:= iconSrc
      , labelAttrs:
          klass_ "decoration-sky-500/30"
      , label:
          text_ display
      }

checkBox :: String -> Nut
checkBox label =
  ( pursx ::
      _ """
    <label class="checkbox">
      <span class="checkbox-label">~label~</span>
      <input type="checkbox" />
    </label>
  """
  )
    ~~ { label: text_ label }
