-- | eg ParsedExpression -> CoreExpression
module JappieLang.Simplify
  (simplify
  )
where

import JappieLang.SyntaxTree.Name
import qualified JappieLang.SyntaxTree.Parsed as Parsed
import JappieLang.SyntaxTree.Parsed(ParsedExpression)
import JappieLang.SyntaxTree.Core(CoreExpression)
import qualified JappieLang.SyntaxTree.Core as Core

data SimplifyIsseus = IsComment
                    | CommentInLambda Name -- nonsensical lambda
                    deriving (Eq, Show)

simplify :: ParsedExpression -> Either SimplifyIsseus CoreExpression
simplify = \case
  Parsed.Var name       -> Right $ Core.Var name
  Parsed.App appL appR  ->
    case (simplify appL, simplify appR) of
      (Left IsComment, Left IsComment) -> Left IsComment
      (Left IsComment, Right right) -> Right right
      (Right left, Left IsComment) -> Right left
      (otherLeft,otherRight) -> Core.App <$> otherLeft <*> otherRight
  Parsed.Lam name body'  -> case simplify body' of
    (Left IsComment) -> Left $ CommentInLambda name -- forbidden cuz the entire body is a comment
    (Left other) -> Left $ other
    (Right body) -> Right $ Core.Lam name body
  Parsed.Comment _text   -> Left IsComment
