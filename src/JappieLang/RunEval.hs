{-# LANGUAGE RecordWildCards #-}
-- | Eg, get's a text or filepathr runs the expression
--   this uses internal evaluation
--   this hooks together the entire language, from parsing to evaluation
--   we use this to assert correctness.
module JappieLang.RunEval
  ( runFilePrint
  , runTextPrint
  , runFileExpr
  , runTextExpr
  )
where

import Data.Bifunctor
import JappieLang.Print
import JappieLang.SyntaxTree.Core
import JappieLang.SyntaxTree.Name
import Data.Text.Lazy
import qualified Data.Text as SText
import Prettyprinter
import Prettyprinter.Render.Terminal
import JappieLang.Eval
import JappieLang.Frontend

data RunErrors = FrontendError FrontendErrors
               | EvalError EvalErrors

printErrors :: RunErrors -> Text
printErrors = printDoc . runErrorsDoc


evalErrorsDoc :: EvalErrors -> Doc AnsiStyle
evalErrorsDoc (ApplyingNameTo name expr) = pretty @Text "applying " <+> (dquotes (prettyName name)) <+> pretty @Text "to " <+> coreToDoc expr

runErrorsDoc :: RunErrors -> Doc AnsiStyle
runErrorsDoc = \case
  FrontendError frontend -> frontendErrorsDoc frontend
  EvalError evalErrors -> evalErrorsDoc evalErrors

printOutput :: Either RunErrors CoreExpression -> Text
printOutput = either printErrors printCoreExpression

runFilePrint :: FilePath -> IO Text
runFilePrint path = fmap printOutput $ runFileExpr path

runTextPrint :: SText.Text -> Text
runTextPrint = printOutput . runTextExpr

runFileExpr :: FilePath -> IO (Either RunErrors CoreExpression)
runFileExpr path = do
  result' <- fileToCoreExpression path
  pure $ do
    xxx <- first FrontendError result'
    first EvalError $ evalFix xxx

runTextExpr :: SText.Text -> Either RunErrors CoreExpression
runTextExpr x = do
  xxx <- first FrontendError $ textToCoreExpression x
  first EvalError $ evalFix xxx
