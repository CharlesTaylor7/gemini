{-# language NoMonomorphismRestriction #-}
{-# options_ghc -Wno-missing-signatures #-}
module Polysemy.State.Optics
  ( (.=), (?=),  (%=)
  , (<.=), (<?=), (<%=)
  , (<<.=), (<<?=), (<<%=)
  ) where

import Relude
import Optics.State.Operators qualified as Optics
import Polysemy
import Polysemy.State as P

infix 4 .=, ?=,  %=, <.=, <?=, <%=, <<.=, <<?=, <<%=

(.=) = (stateTransformerToSem .) . (Optics..=)
(?=) = (stateTransformerToSem .) . (Optics.?=)
(%=) = (stateTransformerToSem .) . (Optics.%=)
(<.=) = (stateTransformerToSem .) . (Optics.<.=)
(<?=) = (stateTransformerToSem .) . (Optics.<?=)
(<%=) = (stateTransformerToSem .) . (Optics.<%=)
(<<.=) = (stateTransformerToSem .) . (Optics.<<.=)
(<<?=) = (stateTransformerToSem .) . (Optics.<<?=)
(<<%=) = (stateTransformerToSem .) . (Optics.<<%=)

stateTransformerToSem :: forall s a r.Member (P.State s) r => StateT s (Sem r) a -> Sem r a
stateTransformerToSem m = do
  s <- P.get
  (a, s') <- runStateT m s
  P.put s'
  pure a
