-- | pretty printing
module JappieLang.Print
  ( printCoreExpression
  )
where

import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name
import Data.Text.Lazy.Builder
import Data.Text.Lazy


printCoreExpression :: CoreExpression -> Text
printCoreExpression = toLazyText . toBuilder


toBuilder :: CoreExpression -> Builder
toBuilder = \case
  Var name -> nameToBuilder name
  App expr1 expr2 -> "(" <> toBuilder expr1 <> " " <> toBuilder expr2 <> ")"
  Lam name body -> fromText "(["
    <> nameToBuilder name
    <> fromText "] "
    <> toBuilder body  <> fromText ")"


nameToBuilder :: Name -> Builder
nameToBuilder (MkName name) = fromText name
