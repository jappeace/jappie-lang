{-# OPTIONS_GHC -Wno-orphans #-}

module TestOrphans where

import Text.Trifecta
import Prettyprinter.Internal
import Prettyprinter.Render.Terminal

instance Eq (Doc AnsiStyle) where
  (==) doca docb = show doca == show docb

deriving instance Eq ErrInfo
deriving instance Eq a => Eq (Result a)
