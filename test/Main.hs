module Main where

import Test.Tasty
import Test.Tasty.HUnit
import JappieLang
import Text.Trifecta
import Prettyprinter.Internal
import Prettyprinter.Render.Terminal.Internal
import Control.Monad.IO.Class

import Data.List(sort)

main :: IO ()
main = defaultMain $ testGroup "tests" [unitTests, langFiles]

toSuccess :: Result a -> a
toSuccess = \case
  Success a -> a
  Failure err -> error $ show err

unitTests :: TestTree
unitTests = testGroup "unit"
  [
    testCase "parse a lambda" $
       toSuccess (parseString parseExpr mempty "([x] x)") @?= Lam (MkName "x") (var "x")
  , testCase "parse a var" $
       toSuccess (parseString parseExpr mempty "x") @?= var "x"
  , testCase "parse an app" $
       toSuccess (parseString parseExpr mempty "(([x] x) ([y] y))") @?= App (Lam (MkName "x") (var "x")) (Lam (MkName "y") (var "y"))
  , testCase "parse a comment" $
       toSuccess (parseString parseExpr mempty "; xxxx") @?= Comment "xxxx"
  , testCase "parse a comment" $
       toSuccess (parseString parseExpr mempty "; xxxx ([x] x)") @?= Comment "xxxx ([x] x)"
  ]

langFiles :: TestTree
langFiles = testGroup "Language files"
  [ testCase "./jappie-lang/lambda.jappie" $ do
      res <- liftIO $ parseFromFile parseExpr "test/jappie-lang/lambda.jappie"
      res @?= Just (Comment "comment")
  ]
