{-# LANGUAGE RecordWildCards #-}
-- | Takes an AST, make an LLVM asm out of it
module JappieLang.CodeGen
  ( toLLVMModule
  , genLLVMAssembly
  , writeTargetAssembly
  )
where

import LLVM.IRBuilder.Monad
import qualified Control.Monad as M
import qualified LLVM.AST.Constant as Constant
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
import GHC.Stack
import qualified LLVM.Target as Target

-- | in goes the language, out comse machine specific assembly
writeTargetAssembly :: FilePath -> IO ByteString
writeTargetAssembly language =
  Target.withHostTargetMachineDefault $ \target -> do
    coreExpr <- handleFrontentErrors =<< fileToCoreExpression language
    asLLVMModule coreExpr $
      FFI.moduleTargetAssembly target

handleFrontentErrors :: Either FrontendErrors CoreExpression -> IO CoreExpression
handleFrontentErrors = \case
    Left err -> error (unpack (printDoc $ frontendErrorsDoc err))
    Right core -> pure core

genLLVMAssembly :: HasCallStack =>FilePath -> IO ByteString
genLLVMAssembly path =
  toLLVMAssembly =<< handleFrontentErrors =<< fileToCoreExpression path

toLLVMAssembly :: HasCallStack =>CoreExpression -> IO ByteString
toLLVMAssembly expression = asLLVMModule expression FFI.moduleLLVMAssembly

asLLVMModule :: HasCallStack =>CoreExpression -> (FFI.Module -> IO a) -> IO a
asLLVMModule expression moduleConsumer =
    withContext $ \emptyContext ->
      FFI.withModuleFromAST emptyContext moduleAst moduleConsumer
    where
      moduleAst = buildModule "Main" $ toLLVMModule expression

toLlvmName :: Jappie.Name -> Name
toLlvmName name' =
  Name (fromString $ (SText.unpack (Jappie.humanName name') <> "_" <> (show (Jappie.shadowBust name'))))


toLLVMModule :: HasCallStack => CoreExpression -> ModuleBuilder ()
toLLVMModule core = do
  exitF <- extern "exit" [IntegerType 8] VoidType
  thunk <- buildSubExpresion MkRuntime core
  M.void $ function "main" [] (IntegerType 32) $ \_x -> do
    thunk
    (M.void $ call exitF [(ConstantOperand (Constant.Int 8 0), [])])


buildSubExpresion ::  MonadModuleBuilder m => Runtime -> CoreExpression -> ModuleBuilder (IRBuilderT m ())
buildSubExpresion runtime = \case
  Var name -> do
    let nameStr = Jappie.humanName name
    callPuts nameStr $ \putsF -> do
      pure $ do
        strConstant <- globalStringPtr (SText.unpack nameStr) (toLlvmName name)
        (M.void $ call putsF [(ConstantOperand strConstant, [])])

  App left right -> do
    one <- buildSubExpresion runtime left
    two <- buildSubExpresion runtime right
    pure $ do
      one
      two
  Lam _name expr -> buildSubExpresion runtime expr

data Runtime = MkRuntime -- TODO delete?

-- | declares a puts of the right size
callPuts :: SText.Text -> (Operand -> ModuleBuilder a) -> ModuleBuilder a
callPuts txt fun = do
  putsF <- extern "puts" [ArrayType (fromIntegral $ SText.length txt) (IntegerType 8) ] VoidType
  fun putsF
