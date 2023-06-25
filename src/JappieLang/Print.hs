-- | pretty printing
module JappieLang.Print
  ( printCoreExpression
  , coreToDoc
  , printDoc
  )
where

import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name
import Data.Text.Lazy
import Prettyprinter
import Prettyprinter.Render.Terminal

printCoreExpression :: CoreExpression -> Text
printCoreExpression = printDoc . coreToDoc

coreToDoc :: CoreExpression -> Doc AnsiStyle
coreToDoc = \case
  Var name -> prettyName name
  App expr1 expr2 -> parens $ coreToDoc expr1 <> space <> coreToDoc expr2
  Lam name body -> parens $ (brackets (prettyName name)) <+> coreToDoc body

printDoc :: Doc AnsiStyle -> Text
printDoc = renderLazy . layoutSmart defaultLayoutOptions
