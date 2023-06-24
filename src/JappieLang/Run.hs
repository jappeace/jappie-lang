{-# LANGUAGE RecordWildCards #-}
-- | Eg, get's a text or filepathr runs the expression
--   this uses internal evaluation
--   this hooks together the entire language, from parsing to evaluation
--   we use this to assert correctness.
module JappieLang.Run
  ( runFilePrint
  , runTextPrint
  , runFileExpr
  , runTextExpr
  )
where

import Data.Bifunctor
import JappieLang.Parser
import Text.Trifecta.Delta
import JappieLang.Print
import JappieLang.SyntaxTree.Parsed
import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name
import Data.Text.Lazy
import qualified Data.Text as SText
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.Trifecta.Result
import JappieLang.Simplify
import JappieLang.Eval

data RunErrors = ParseError ErrInfo
               | SimplifyErrors SimplifyIsseus
               | EvalError EvalErrors

printErrors :: RunErrors -> Text
printErrors =
  renderLazy . layoutSmart defaultLayoutOptions . runErrorsDoc

parseErrorDoc :: ErrInfo -> Doc AnsiStyle
parseErrorDoc ErrInfo{..} =
  vsep
  [ _errDoc
  , vsep  ((punctuate comma ((pretty @Text "at" <+>) . prettyDelta <$> _errDeltas)))
  ]


evalErrorsDoc :: EvalErrors -> Doc AnsiStyle
evalErrorsDoc (ApplyingNameTo name expr) = pretty @Text "applying " <+> (dquotes (prettyName name)) <+> pretty @Text "to " <+> coreToDoc expr

runErrorsDoc :: RunErrors -> Doc AnsiStyle
runErrorsDoc = \case
  ParseError parseError -> parseErrorDoc parseError
  SimplifyErrors simplify' -> simplifyDoc simplify'
  EvalError evalErrors -> evalErrorsDoc evalErrors

printOutput :: Either RunErrors CoreExpression -> Text
printOutput = either printErrors printCoreExpression

runFilePrint :: FilePath -> IO Text
runFilePrint path = fmap printOutput $ runFileExpr path

runTextPrint :: SText.Text -> Text
runTextPrint = printOutput . runTextExpr

runFileExpr :: FilePath -> IO (Either RunErrors CoreExpression)
runFileExpr path =
  runResultExpression <$> parseFile path

runTextExpr :: SText.Text -> Either RunErrors CoreExpression
runTextExpr x = runResultExpression $ parseText x

runResultExpression :: (Result ParsedExpression) -> Either RunErrors CoreExpression
runResultExpression result = do
  parsedExpr <- foldResult (Left . ParseError) Right result
  coreExpr   <- first SimplifyErrors $ simplify parsedExpr
  first EvalError $ eval coreExpr
