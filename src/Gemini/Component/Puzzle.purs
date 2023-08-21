module Gemini.Component.Puzzle
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
    [ ]
    [ text_ "Hello" ]

