-- | The result of the parser
--   This contains the entire surface language
--   Comments will be included for example
module JappieLang.SyntaxTree.Parsed
  ( ParsedExpression (..)
  , var
  )
where

import JappieLang.SyntaxTree.Name
import Data.Text(Text)

data ParsedExpression
          = Var Name
          | App ParsedExpression ParsedExpression
          | Lam Name ParsedExpression
          | Comment Text
          deriving (Eq, Show)

var :: Text -> ParsedExpression
var = Var . MkName

instance Semigroup ParsedExpression where
  (<>) a b = App a b

instance Monoid ParsedExpression where
  mempty = Comment ""
