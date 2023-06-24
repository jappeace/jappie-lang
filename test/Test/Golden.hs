
module Test.Golden(goldenTests) where

import Test.Tasty
import TestOrphans()
import qualified Data.Text.Lazy.Encoding as Text
import JappieLang.Run
import Test.Tasty.Golden


goldenTests :: IO TestTree
goldenTests = pure goldenRuns

goldenRuns :: TestTree
goldenRuns = testGroup "Golden runs" [
  goldenVsString "eval-identity" "test/golden/identity.expected" (Text.encodeUtf8 <$> runFilePrint "test/golden/identity.jappie")
  ]
