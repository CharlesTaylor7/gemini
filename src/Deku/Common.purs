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

import Deku.Attribute (class Attr, attr, xdata, (!:=))
import Deku.Attributes (klass, klass_, style, style_)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.Extra as Extra
import Deku.Hooks (useAff, useEffect, useRef, useState, useState')
import Deku.Pursx (pursx, (~~))
