module JappieLang.Expression
  ( Expression(..)
  , Name(..)
  , var
  )
where

import Data.Text(Text)
import Data.String

newtype Name = MkName Text
  deriving stock (Eq, Show)
  deriving newtype IsString

data Expression = Var Name
          | App Expression Expression
          | Lam Name Expression
          | Comment Text
          deriving (Eq, Show)


var :: Text -> Expression
var = Var . MkName
