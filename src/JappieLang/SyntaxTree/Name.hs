module JappieLang.SyntaxTree.Name
  ( Name(..)
  )
where

import Data.Text(Text)
import Data.String

newtype Name = MkName Text
  deriving stock (Eq, Show)
  deriving newtype IsString
