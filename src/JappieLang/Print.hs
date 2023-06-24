-- | pretty printing
module JappieLang.Print
  ( printCoreExpression
  , coreToDoc
  )
where

import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name
import Data.Text.Lazy
import Prettyprinter
import Prettyprinter.Render.Terminal

printCoreExpression :: CoreExpression -> Text
printCoreExpression = renderLazy . layoutSmart defaultLayoutOptions . coreToDoc

coreToDoc :: CoreExpression -> Doc AnsiStyle
coreToDoc = \case
  Var name -> nameToBuilder name
  App expr1 expr2 -> parens $ coreToDoc expr1 <> space <> coreToDoc expr2
  Lam name body -> parens $ (brackets (nameToBuilder name)) <+> coreToDoc body

nameToBuilder :: Name -> Doc AnsiStyle
nameToBuilder (MkName name) = pretty name
