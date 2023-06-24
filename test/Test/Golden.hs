
module Test.Golden(goldenTests) where

import Test.Tasty
import TestOrphans()
import qualified Data.Text.Lazy.Encoding as Text
import JappieLang.Run
import Test.Tasty.Golden


goldenTests :: IO TestTree
goldenTests = do
  filePaths <- fmap (fmap $ (takeWhile (/= '.'))) $ findByExtension [".expected"] "test/golden"
  print filePaths
  pure $ testGroup "Golden runs" (goldenRun <$> filePaths)

goldenRun :: FilePath -> TestTree
goldenRun path =
  goldenVsString path (path <> ".expected") (Text.encodeUtf8 <$> runFilePrint (path <>".jappie"))
