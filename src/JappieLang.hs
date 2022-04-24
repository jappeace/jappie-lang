module JappieLang
  ( parseExpr
  , var
  , Expr(..)
  , Name(..)
  )
where

import Text.Trifecta
import Text.Parser.Combinators
import Data.Text
import Control.Applicative
import Text.Parser.Token

newtype Name = MkName Text
  deriving (Eq, Show)

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          | Comment Text
          deriving (Eq, Show)
-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
parseExpr :: Parser Expr
parseExpr = parseLam <|> parseVar <|> comment

parseApp :: Parser Expr
parseApp = parens $ do
  one <- parseExpr
  two <- parseExpr
  pure (App one two)


parseLam :: Parser Expr
parseLam = parens $ do
        Var name <- brackets parseVar
        body <- parseExpr
        pure (Lam name body)

parseVar :: Parser Expr
parseVar = var <$> stringLiteral

var :: Text -> Expr
var = Var . MkName

-- https://hackage.haskell.org/package/parsers-0.12.10/docs/Text-Parser-Combinators.html#v:endBy
comment :: Parser Expr
comment = do
  symbolic ';'
  pure (Comment "")
