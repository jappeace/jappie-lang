module JappieLang.Eval
  (eval
  , EvalError(..)
  )
where

import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name

data EvalError = ApplyingNameTo Name CoreExpression
  deriving (Eq, Show)

eval :: CoreExpression -> Either EvalError CoreExpression
eval = \case
  Var name -> Right (Var name)
  App expr1 expr2 -> apply expr1 expr2
  Lam name expr -> Right (Lam name expr)

apply :: CoreExpression -> CoreExpression -> Either EvalError CoreExpression
apply appliedTo appliedWith = case appliedTo of
  Var name -> Left (ApplyingNameTo name appliedWith) -- eg, a function call?
  -- eg (([x] x) ([y] y) ([z] z))
  App expr1 expr2 ->
    (apply expr1 expr2) >>= \x -> apply x appliedWith
  Lam name expr -> (substitute name expr appliedWith)

-- so all occurences of Name in first argument get replaced by the second argument
substitute :: Name -> CoreExpression -> CoreExpression -> Either EvalError CoreExpression
substitute name appliedTo appliedWith = case appliedTo of
  Var name' -> if name == name' then Right appliedWith else Right (Var name')
  App expr1 expr2 -> App <$> (substitute name expr1 appliedWith) <*> (substitute name expr2 appliedWith)
  Lam name' expr -> Lam name' <$> (substitute name expr appliedWith)
