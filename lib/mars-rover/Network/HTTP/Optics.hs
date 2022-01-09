{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}
module Network.HTTP.Optics () where

-- http-client
import Network.HTTP.Client (HttpException, HttpExceptionContent, Response)

-- http-types
import Network.HTTP.Types (Status)

-- optics
import Optics (makeFieldLabels, makePrismLabels)


makePrismLabels ''HttpExceptionContent
makePrismLabels ''HttpException
makeFieldLabels ''Response
makeFieldLabels ''Status
