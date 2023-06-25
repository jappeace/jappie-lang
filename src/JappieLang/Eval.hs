module JappieLang.Eval
  (evalStep
  , evalFix
  , EvalErrors(..)
  )
where

import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name

data EvalErrors = ApplyingNameTo Name CoreExpression
  deriving (Eq, Show)

-- | keeps on performing evaluation steps untill the expression
--   is the same in 2 consequative steps
evalFix :: CoreExpression -> Either EvalErrors CoreExpression
evalFix expr =
  let
    once = evalStep expr
    twice = evalStep expr >>= evalStep
  in
    if once == twice then once else once >>= evalFix


-- | performs a single evaluation step
evalStep :: CoreExpression -> Either EvalErrors CoreExpression
evalStep = \case
  Var name -> Right (Var name)
  App expr1 expr2 -> apply expr1 expr2
  Lam name expr -> Lam name <$> (evalStep expr)

apply :: CoreExpression -> CoreExpression -> Either EvalErrors CoreExpression
apply appliedTo appliedWith = case appliedTo of
  Var name -> Left (ApplyingNameTo name appliedWith) -- eg, a function call?
  -- eg (([x] x) ([y] y) ([z] z))
  App expr1 expr2 ->
    (apply expr1 expr2) >>= \x -> apply x appliedWith
  Lam name expr -> Right (substitute name expr appliedWith)

-- | so all occurences of Name in first argument get replaced by the second argument
substitute :: Name -> CoreExpression -> CoreExpression -> CoreExpression
substitute name appliedTo appliedWith = case appliedTo of
  Var name' -> if name == name' then appliedWith else (Var name')
  App expr1 expr2 -> App (substitute name expr1 appliedWith) (substitute name expr2 appliedWith)
  Lam name' expr -> Lam name' (substitute name expr appliedWith)
