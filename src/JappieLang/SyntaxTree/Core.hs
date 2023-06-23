-- | Core syntax tree
module JappieLang.SyntaxTree.Core
  ( CoreExpression (..)
  , var
  )
where

import JappieLang.SyntaxTree.Name
import Data.Text(Text)

data CoreExpression = Var Name
          | App CoreExpression CoreExpression
          | Lam Name CoreExpression
          deriving (Eq, Show)

var :: Text -> CoreExpression
var = Var . MkName
