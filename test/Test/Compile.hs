
module Test.Compile(llvmTests) where

import System.Process.Typed
import Test.Tasty
import TestOrphans()
import Test.Tasty.Golden
import JappieLang.CodeGen
import qualified Data.ByteString.Lazy as LBS
import JappieLang.Cli

llvmTests :: IO TestTree
llvmTests = do
  filePaths <- fmap (fmap $ (takeWhile (/= '.'))) $ findByExtension [".expected"] "test/golden-llvm-asm"
  print filePaths
  pure $ testGroup "Golden runs" (goldenRun <$> filePaths)

goldenRun :: FilePath -> TestTree
goldenRun path =
  goldenVsString path (path <> ".expected") $ do
    compile $ MkCompileOptions {
      inputFile = (path <>".jappie")
      , outputFile = "a.out"
      }
    readProcessStdout_ "./a.out"
