-- | eg ParsedExpression -> CoreExpression
module JappieLang.Simplify
  (simplify
  , SimplifyIsseus(..)
  , simplifyDoc
  )
where

import JappieLang.SyntaxTree.Name
import qualified JappieLang.SyntaxTree.Parsed as Parsed
import JappieLang.SyntaxTree.Parsed(ParsedExpression)
import JappieLang.SyntaxTree.Core(CoreExpression)
import qualified JappieLang.SyntaxTree.Core as Core
import Data.Text(Text)
import Prettyprinter
import Prettyprinter.Render.Terminal

-- TODO keep track of where these occured
simplifyDoc :: SimplifyIsseus -> Doc AnsiStyle
simplifyDoc = \case
    IsComment comment -> pretty @Text "is comment: " <+> dquotes (pretty comment)
    EmptyLambda name text -> pretty @Text "empty lambda: " <+> prettyName name <+> "filled with " <+> dquotes (pretty text)

data SimplifyIsseus = IsComment Text
                    | EmptyLambda Name Text -- nonsensical lambda
                    deriving (Eq, Show)

simplify :: ParsedExpression -> Either SimplifyIsseus CoreExpression
simplify = \case
  Parsed.Var name       -> Right $ Core.Var name
  Parsed.App appL appR  ->
    case (simplify appL, simplify appR) of
      (Left (IsComment txt), Left (IsComment txt2)) -> Left (IsComment (txt <> ";" <> txt2))
      (Left (IsComment _), Right right) -> Right right
      (Right left, Left (IsComment _)) -> Right left
      (otherLeft,otherRight) -> Core.App <$> otherLeft <*> otherRight
  Parsed.Lam name body'  -> case simplify body' of
    (Left (IsComment txt)) -> Left $ EmptyLambda name txt -- forbidden cuz the entire body is a comment
    (Left other) -> Left $ other
    (Right body) -> Right $ Core.Lam name body
  Parsed.Comment text   -> Left $ IsComment text
