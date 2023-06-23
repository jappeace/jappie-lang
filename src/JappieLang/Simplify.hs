-- | eg ParsedExpression -> CoreExpression
module JappieLang.Simplify
  (simplify
  )
where

import Control.Monad
import Text.Parser.Token.Style
import Text.Trifecta
import qualified Data.Text as T
import Control.Applicative
import qualified JappieLang.SyntaxTree.Parsed as Parsed
import JappieLang.SyntaxTree.Parsed(ParsedExpression)
import JappieLang.SyntaxTree.Core(CoreExpression)
import qualified JappieLang.SyntaxTree.Core as Core

data SimplifyIsseus = IsComment
                    | CommentInLambda Name -- nonsensical lambda

simplify :: ParsedExpression -> Either SimplifyIsseus CoreExpression
simplify = \case
  Parsed.Var name       -> Right $ Core.Var name
  Parsed.App appL appR  ->
    case (simplify appL, simplify appR) of
      (Left IsComment, Left IsComment) -> Left IsComment
      (left, right) -> App <$> left <*> right <|> left <|> right
  Parsed.Lam name body  -> case simplify body of
    (Left IsComment) -> Left $ CommentInLambda name
    (Right body) -> Right $ Core.Lam name body
  Parsed.Comment text   -> Left IsComment
