{-# LANGUAGE RecordWildCards #-}
-- | This module defines all steps up till core,
--   which then cane be used in some backend, such as eval or codegen
module JappieLang.Frontend
  ( fileToCoreExpression
  , textToCoreExpression
  , FrontendErrors(..)
  , frontendErrorsDoc
  )
where

import Data.Bifunctor
import JappieLang.Parser
import Text.Trifecta.Delta
import JappieLang.SyntaxTree.Parsed
import JappieLang.SyntaxTree.Core
import Data.Text.Lazy
import qualified Data.Text as SText
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.Trifecta.Result
import JappieLang.Simplify
import JappieLang.Rename

parseErrorDoc :: ErrInfo -> Doc AnsiStyle
parseErrorDoc ErrInfo{..} =
  vsep
  [ _errDoc
  , vsep  ((punctuate comma ((pretty @Text "at" <+>) . prettyDelta <$> _errDeltas)))
  ]


frontendErrorsDoc :: FrontendErrors -> Doc AnsiStyle
frontendErrorsDoc = \case
  ParseError parseError -> parseErrorDoc parseError
  SimplifyErrors simplify' -> simplifyDoc simplify'

data FrontendErrors = ParseError ErrInfo
                | SimplifyErrors SimplifyIsseus

fileToCoreExpression :: FilePath -> IO (Either FrontendErrors CoreExpression)
fileToCoreExpression path =
  resultToCoreExpression <$> parseFile path

textToCoreExpression :: SText.Text -> Either FrontendErrors CoreExpression
textToCoreExpression x = resultToCoreExpression $ parseText x

resultToCoreExpression :: (Result ParsedExpression) -> Either FrontendErrors CoreExpression
resultToCoreExpression result = do
  parsedExpr <- foldResult (Left . ParseError) Right result
  coreExpr   <- first SimplifyErrors $ simplify parsedExpr
  pure $ rename coreExpr
