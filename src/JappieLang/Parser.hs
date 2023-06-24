module JappieLang.Parser
  ( parseText
  , parseFile
  -- ** trifecata
  , parseExpression
  , parseExpressions
  )
where

import Control.Monad
import Text.Parser.Token.Style
import Text.Trifecta
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text(Text)
import Control.Applicative
import JappieLang.SyntaxTree.Parsed
import JappieLang.SyntaxTree.Name
import Data.Foldable(foldl')

-- TODO get the delta for all expressions and record in ast
-- https://hackage.haskell.org/package/trifecta-2.1.2/docs/Text-Trifecta-Combinators.html#v:position
-- or we can make an index per occured expression and create
-- an external lookup map of deltas.
--
-- I think that's better since the Delta's have little to do
-- with evaluation or codegen, it's only usefull for error reporting

parseFile :: FilePath -> IO (Result ParsedExpression)
parseFile path = fmap toTree <$> parseFromFileEx parseExpressions path

parseText :: Text -> Result ParsedExpression
parseText txt = toTree <$> (parseByteString parseExpressions mempty (Text.encodeUtf8 txt))

-- | Makes a tree out of a list of parsedExpressions.
--   this how to deal with files
toTree :: [ParsedExpression] -> ParsedExpression
toTree = \case
  [] -> mempty
  one:[] -> one
  (one:many') -> foldl' App one many'

parseExpressions :: Parser [ParsedExpression]
parseExpressions = some parseExpression

-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
parseExpression :: Parser ParsedExpression
parseExpression = parseVar <|> try parseApp <|> try parseLam <|> comment

parseApp :: Parser ParsedExpression
parseApp = parens $ do
  one <- parseExpression
  two <- parseExpression
  pure (App one two)

parseLam :: Parser ParsedExpression
parseLam = parens $ do
  bindings <- brackets (some parseIdent)
  body <- some parseExpression
  pure $ foldr Lam (toTree body) bindings


parseVar :: Parser ParsedExpression
parseVar = Var <$> parseIdent

parseIdent :: Parser Name
parseIdent = MkName <$> ident idStyle

-- https://hackage.haskell.org/package/parsers-0.12.10/docs/Text-Parser-Combinators.html#v:endBy
comment :: Parser ParsedExpression
comment = do
  void $ char ';'
  chars <- many (notChar '\n')
  void newline <|> eof
  pure (Comment (Text.pack (chars)))

idStyle :: IdentifierStyle Parser
idStyle    = emptyIdents
