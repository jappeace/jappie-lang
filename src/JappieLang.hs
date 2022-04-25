module JappieLang
  ( parseExpr
  , parseExprs
  , var
  , Expr(..)
  , Name(..)
  )
where

import Debug.Trace
import Control.Monad
import Text.Parser.Token.Style
import Text.Trifecta
import Text.Parser.Combinators
import qualified Data.Text as T
import Data.Text(Text)
import Control.Applicative
import Text.Parser.Token

newtype Name = MkName Text
  deriving (Eq, Show)

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          | Comment Text
          deriving (Eq, Show)

parseExprs :: Parser [Expr]
parseExprs = some parseExpr

-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
parseExpr :: Parser Expr
parseExpr = parseVar <|> try parseApp <|> try parseLam <|> comment

parseApp :: Parser Expr
parseApp = parens $ do
  one <- parseExpr
  two <- parseExpr
  pure (App one two)

parseLam :: Parser Expr
parseLam = parens $ do
  bindings <- brackets parseIdent
  body <- parseExpr
  pure $ Lam bindings body

parseVar :: Parser Expr
parseVar = Var <$> parseIdent

parseIdent :: Parser Name
parseIdent = MkName <$> ident idStyle

var :: Text -> Expr
var = Var . MkName

-- https://hackage.haskell.org/package/parsers-0.12.10/docs/Text-Parser-Combinators.html#v:endBy
comment :: Parser Expr
comment = do
  char ';'
  chars <- many (notChar '\n')
  void newline <|> eof
  pure (Comment (T.pack (chars)))

idStyle    = emptyIdents
