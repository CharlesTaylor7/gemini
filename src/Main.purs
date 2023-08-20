module Main where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Deku.Control (text)
import Deku.Listeners as Event
import Deku.Toplevel (runInBody, runInBody')
import Deku.Core (Nut)
import Deku.Do as Deku
import Deku.DOM as H
import Deku.Pursx (pursx, (~~))
import Deku.Attributes as Attr
import Deku.Attribute (xdata)
import Deku.Hooks (useState)
import Deku.Extra (keys)


mainDev :: Effect (Effect Unit)
mainDev = do
  log $ keys { apple: "334", banana: "32" }
  runInBody' app


main :: Effect Unit
main = runInBody app

app :: Nut
app = Deku.do
  setCounter /\ counter <- useState 0
  H.button
    [ Event.click $ counter <#> add 1 >>> setCounter
    , Attr.klass_ "bg-blue-200 p-2 border rounded-lg"
    , Attr.klass_ "m-2"
    , pure (xdata "testid" "my-testid")
    ]
    [ text (show <$> counter)
    , (pursx :: _ Template) ~~
        { adj: 
            text $
            counter <#> \c -> if c `mod` 2 == 0 then "even" else "odd"
        }
    ]

type Template = "<span>~adj~</span>"
