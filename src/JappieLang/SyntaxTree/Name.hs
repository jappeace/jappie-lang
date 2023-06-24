module JappieLang.SyntaxTree.Name
  ( Name(..)
  , prettyName
  )
where

import Data.Text(Text)
import Data.String
import Prettyprinter
import Prettyprinter.Render.Terminal

newtype Name = MkName Text
  deriving stock (Eq, Show)
  deriving newtype IsString

prettyName :: Name -> Doc AnsiStyle
prettyName (MkName name) = dquotes (pretty name)
