module Gemini.Component.App
  ( component
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Deku.Control (text, text_)
import Deku.Listeners as Event
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Deku.Hooks (useState)
import Deku.Extra (className)

import Data.Gemini as Gemini 

import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle
import Gemini.Types (initialStore, Store)


component :: Nut
component = Deku.do
  setState /\ store <- useState initialStore 
  D.div
    [ klass_  "gemini-app" 
    --, D.Autofocus !:= true
    --, D.Tabindex !:= 0
    ]
    [ D.div
        [ klass_ "main-panel"]
        [ header 
        , Puzzle.component store
        , footer 
        ]
    , D.div [ klass_ "right-panel"]
      [ 
        --recordedMovesPanel 
      ]
    ]


header :: Nut 
header =
  D.div [ klass_ "header" ]
  [ D.div [ klass_ "control-panel" ]
    [ D.button
      [ klass_ "action-button" ]
      [ text_ "Scramble" ]
    , D.button
      [ klass_ "action-button" ]
      [ text_ "Undo" ]
    ] 
  ]


footer :: Nut
footer =
  D.div [ klass_ "footer" ]
  [ D.div [ klass_ "links" ]
    [ hyperlink
      "./github.png"
      "https://github.com/CharlesTaylor7/gemini"
      "View Source"
    ]
  , D.div [ klass_ "explain-controls" ]
      [ D.div__ "Q: Rotate left disk counter clockwise"
      , D.div__ "W: Rotate left disk clockwise"
      , D.div__ "T: Rotate center disk counter clockwise"
      , D.div__ "Y: Rotate center disk clockwise"
      , D.div__ "O: Rotate right disk counter clockwise"
      , D.div__ "P: Rotate right disk clockwise"
      ]
  ]

hyperlink :: String -> String -> String -> Nut
hyperlink iconSrc linkUrl display =
  (pursx :: _ """
    <a class="link" target="_blank" rel="noopener noreferrer" ~linkHref~>
        <img ~src~ />
        ~label~
    </a>
  """) ~~ 
    { linkHref: href_ linkUrl
    , src: D.Src !:= iconSrc
    , label: text_ display
    }
   

checkBox :: String -> Nut
checkBox label =
  (pursx :: _ """
    <label class="checkbox">
      <span class="checkbox-label">~label~</span>
      <input type="checkbox" />
    </label>
  """) ~~ { label: text_ label }
    
