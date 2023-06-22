module JappieLang.Parser
  ( parseExpression
  , parseExpressions
  )
where

import Control.Monad
import Text.Parser.Token.Style
import Text.Trifecta
import qualified Data.Text as T
import Control.Applicative
import JappieLang.Expression

parseExpressions :: Parser [Expression]
parseExpressions = some parseExpression

-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
parseExpression :: Parser Expression
parseExpression = parseVar <|> try parseApp <|> try parseLam <|> comment

parseApp :: Parser Expression
parseApp = parens $ do
  one <- parseExpression
  two <- parseExpression
  pure (App one two)

parseLam :: Parser Expression
parseLam = parens $ do
  bindings <- brackets parseIdent
  body <- parseExpression
  pure $ Lam bindings body

parseVar :: Parser Expression
parseVar = Var <$> parseIdent

parseIdent :: Parser Name
parseIdent = MkName <$> ident idStyle

-- https://hackage.haskell.org/package/parsers-0.12.10/docs/Text-Parser-Combinators.html#v:endBy
comment :: Parser Expression
comment = do
  void $ char ';'
  chars <- many (notChar '\n')
  void newline <|> eof
  pure (Comment (T.pack (chars)))

idStyle :: IdentifierStyle Parser
idStyle    = emptyIdents
