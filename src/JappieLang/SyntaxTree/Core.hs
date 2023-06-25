-- | This tree can be evaluated. It represents the language in
--   it's simpelest form.
--   constructs such as comments have been filtered out at this point.
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
var = Var . mkName
