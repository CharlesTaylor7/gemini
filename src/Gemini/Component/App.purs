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

import Gemini.Env (Env)


component :: Nut
component = Deku.do
  D.div
    [ className 
      [ "gemini-app" /\ pure true
      , "dragging" /\ pure false -- isn't (#drag % _Nothing) store
      , "justify-between" /\ pure false 
      , "justify-center" /\ pure true 
      ]
    --, D.Autofocus !:= true
    --, D.Tabindex !:= 0
    ]
    [ D.div
        [ klass_ "main-panel"]
        [ header 
        --, geminiView 
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
    [ D.div 
      [ klass_ "button-group options"]
      [ checkBox "Labels"
      , checkBox "Keyboard controls?"
      , checkBox "Bot controls?"
      ]
    ]
  ]


footer :: Nut
footer =
  D.div [ klass_ "footer" ]
  [ D.div [ klass_ "links" ]
    [ hyperlink
      "/icons/Github.png"
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
  D.a
    [ klass_ "link"
    , href_ linkUrl
    , D.Target !:= "_blank"
    , D.Rel !:= "noopener noreferrer"
    ]
    [ D.img [ D.Src !:= iconSrc ] []
    , text_ display
    ]

checkBox :: String -> Nut
checkBox label =
  D.label
    [ klass_ "checkbox" ]
    [ D.span
        [ klass_ "checkbox-label" ]
        [ text_ label ]
    , (pursx :: _ "<input type=\"checkbox\" />") ~~ {}
    ]
