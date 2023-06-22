module JappieLang.Eval
  (eval
  )
where

import Data.Text(Text)
import JappieLang.Expression

data EvalError = FreeVariable Name
               | ApplyingNameTo Name Expression
               | ApplyingCommentTo Text Expression
  deriving (Eq, Show)

eval :: Expression -> Either EvalError Expression
eval = \case
  Var name -> Right (Var name)
  Comment txt -> Right (Comment txt)
  App expr1 expr2 -> apply expr1 expr2
  Lam name expr -> Right expr

apply :: Expression -> Expression -> Either EvalError Expression
apply appliedTo appliedWith = case appliedTo of
  Var name -> Left (ApplyingNameTo name appliedTo)
  Comment txt -> Left (ApplyingCommentTo txt appliedTo)
  App expr1 expr2 -> (apply expr1 expr2) >>= \x -> apply x appliedWith
  Lam name expr -> (substitute name expr appliedWith)

-- so all occurences of Name in first argument get replaced by the second argument
substitute :: Name -> Expression -> Expression -> Either EvalError Expression
substitute name appliedTo appliedWith = case appliedTo of
  Var name' -> if name == name' then Right appliedWith else Right (Var name')
  Comment txt -> Right (Comment txt)
  App expr1 expr2 -> App <$> (substitute name expr1 appliedWith) <*> (substitute name expr2 appliedWith)
  Lam name expr -> Lam name <$> (substitute name expr appliedWith)
