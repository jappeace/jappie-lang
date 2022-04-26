module JappieLang.Expression
  ( Expression(..)
  , Name(..)
  , var
  )
where

import Control.Monad
import Text.Parser.Token.Style
import Text.Trifecta
import qualified Data.Text as T
import Data.Text(Text)
import Control.Applicative

newtype Name = MkName Text
  deriving (Eq, Show)

data Expression = Var Name
          | App Expression Expression
          | Lam Name Expression
          | Comment Text
          deriving (Eq, Show)


var :: Text -> Expression
var = Var . MkName
