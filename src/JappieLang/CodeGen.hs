{-# LANGUAGE RecordWildCards #-}
-- | Takes an AST
module JappieLang.CodeGen
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
import qualified JappieLang.SyntaxTree.Name as Jappie
import Data.Text.Lazy
import qualified Data.Text as SText
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.Trifecta.Result
import JappieLang.Simplify
import JappieLang.Eval
import JappieLang.Rename
import LLVM.IRBuilder.Module

toLlvmName :: Jappie.Name -> Name
toLlvmName Jappie.MkName{..} =
  Name (fromString $ (unpack humanName <> "_" <> (show shadowBust)))

toLLVMModule :: CoreExpression -> ModuleBuilder ()
toLLVMModule = \case
  Var name -> do
    strConstant <- globalStringPtr (humanName name) (toLlvmName name)
    function "main" [] (IntegerType 32) $ \_x ->
      call (LocalReference (IntegerType 32) "puts") [(LocalReference ""
  App left right -> pure () -- TODO figure out application
  Lam name expr -> pure ()
