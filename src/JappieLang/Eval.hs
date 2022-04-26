module JappieLang.Eval
  (eval
  )
where

import JappieLang.Expression

newtype EvalError = FreeVariable Name
  deriving (Eq, Show)

eval :: Expression -> Either EvalError Expression
eval = \case
  Var name -> Right (Var name)
  Comment txt -> Right (Comment txt)
  App expr1 expr2 -> Right expr1
  Lam name expr -> Right expr
