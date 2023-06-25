{-# LANGUAGE RecordWildCards #-}
-- | Takes an AST, make an LLVM asm out of it
module JappieLang.CodeGen
  ( toLLVMModule
  , genLLVMAssembly
  )
where

import qualified Control.Monad as M
import Data.String
import JappieLang.Print
import JappieLang.SyntaxTree.Core
import qualified JappieLang.SyntaxTree.Name as Jappie
import Data.Text.Lazy
import qualified Data.Text as SText
import LLVM.IRBuilder.Module
import LLVM.AST.Type
import LLVM.AST.Operand
import LLVM.IRBuilder.Instruction
import qualified LLVM.Module as FFI
import Data.ByteString(ByteString)
import LLVM.AST.Name
import LLVM.Context
import JappieLang.Frontend

genLLVMAssembly :: FilePath -> IO ByteString
genLLVMAssembly path = do
  result' <- fileToCoreExpression path
  case result' of
    Left err -> error (unpack (printDoc $ frontendErrorsDoc err))
    Right core -> toLLVMAssembly core

toLLVMAssembly :: CoreExpression -> IO ByteString
toLLVMAssembly expression =
    withContext $ \emptyContext ->
      FFI.withModuleFromAST emptyContext moduleAst FFI.moduleLLVMAssembly
    where
      moduleAst = buildModule "Main" $ toLLVMModule expression

toLlvmName :: Jappie.Name -> Name
toLlvmName name' =
  Name (fromString $ (SText.unpack (Jappie.humanName name') <> "_" <> (show (Jappie.shadowBust name'))))


toLLVMModule :: CoreExpression -> ModuleBuilder ()
toLLVMModule = \case
  Var name -> do
    -- extern puts?
    M.void $ function "main" [] (IntegerType 32) $ \_x -> do
      strConstant <- globalStringPtr (SText.unpack $ Jappie.humanName name) (toLlvmName name)
      (M.void $ call (LocalReference (IntegerType 32) "puts") [(ConstantOperand strConstant, [])])
  App _left _right -> pure () -- TODO figure out application
  Lam _name _expr -> pure ()
