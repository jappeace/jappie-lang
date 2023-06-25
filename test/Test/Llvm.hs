
module Test.Llvm(llvmTests) where

import Test.Tasty
import TestOrphans()
import Test.Tasty.Golden
import JappieLang.CodeGen
import qualified Data.ByteString.Lazy as LBS

llvmTests :: IO TestTree
llvmTests = do
  filePaths <- fmap (fmap $ (takeWhile (/= '.'))) $ findByExtension [".expected"] "test/golden-llvm-asm"
  print filePaths
  pure $ testGroup "Golden runs" (goldenRun <$> filePaths)

goldenRun :: FilePath -> TestTree
goldenRun path =
  goldenVsString path (path <> ".expected") (LBS.fromStrict <$> genLLVMAssembly (path <>".jappie"))
