module Demo 
  ( component
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Deku.Control (text)
import Deku.Listeners as Event
import Deku.Core (Nut)
import Deku.Do as Deku
import Deku.DOM as H
import Deku.Pursx (pursx, (~~))
import Deku.Attributes as Attr
import Deku.Attribute (xdata)
import Deku.Hooks (useState)


component :: Nut
component = Deku.do
  setCounter /\ counter <- useState 0
  H.button
    [ Event.click $ counter <#> add 1 >>> setCounter
    , Attr.klass_ "bg-blue-200 p-2 border rounded-lg"
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