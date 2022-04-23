module JappieLang
  ( main
  )
where

import Text.Trifecta


main :: IO ()
main = putStrLn "hello, world ss"

data Expr = Var String
          | App Expr Expr
          | Lam Name Expr
          | Comment

-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
parseExpr :: Parser Expr
parseExpr = parseLam <|> parseLit <|> comment

parseApp :: Parser Expr
parseApp = parens $ do
  one <- parseExpr
  two <- parseExpr
  pure (App one two)


parseLam :: Parser Expr
parseLam = parens $ do
        Var name <- brackets parseLit
        body <- parseExpr
        pure (Lam name body)

parseLit :: Parser Expr
parseLit = Var <$> word

-- https://hackage.haskell.org/package/parsers-0.12.10/docs/Text-Parser-Combinators.html#v:endBy
comment :: Parser Expr
comment = do
  symbolic ';'
  pure Comment
