-- | Reexport most commonly used utilities
module Deku.Common
  ( module Deku.Control
  , module Deku.Core
  , module Deku.Pursx
  , module Deku.Attribute
  , module Deku.Attributes
  , module Deku.Hooks
  , module Extra
  ) where

import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.Pursx (pursx, (~~))
import Deku.Attributes (klass, klass_)
import Deku.Attribute (xdata, (!:=), attr, class Attr)
import Deku.Hooks (useEffect, useRef)
import Deku.Extra as Extra
