module Main where

import Test.Tasty
import Test.Tasty.HUnit
import JappieLang.Parser
import Text.Trifecta
import Control.Monad.IO.Class
import JappieLang.Expression
import JappieLang.Eval

main :: IO ()
main = defaultMain unitTests

toSuccess :: Result a -> a
toSuccess = \case
  Success a -> a
  Failure err' -> error $ show err'

unitTests :: TestTree
unitTests = testGroup "tests" [
  testGroup "parser"
  [ parserUnit
  , langFiles
  ],
  testGroup "eval"
  [testCase "App" $
    eval (App (Lam (MkName "x") (var "x")) (Lam (MkName "y") (var "y"))) @?= Right (Lam (MkName "y") (var "y"))
  ]
  ]


parserUnit :: TestTree
parserUnit =  testGroup "unit" [
    testCase "parse a lambda" $
       toSuccess (parseString parseExpression mempty "([x] x)") @?= Lam (MkName "x") (var "x")
  , testCase "parse a var" $
       toSuccess (parseString parseExpression mempty "x") @?= var "x"
  , testCase "parse an app" $
       toSuccess (parseString parseExpression mempty "(([x] x) ([y] y))") @?= App (Lam (MkName "x") (var "x")) (Lam (MkName "y") (var "y"))
  , testCase "parse a comment xxxx" $
       toSuccess (parseString parseExpression mempty "; xxxx") @?= Comment " xxxx"
  , testCase "parse a comment with lam" $
       toSuccess (parseString parseExpression mempty "; xxxx ([x] x)") @?= Comment " xxxx ([x] x)"
  , testCase "parse a comment empty" $
       toSuccess (parseString parseExpression mempty ";") @?= Comment ""
  ]

langFiles :: TestTree
langFiles = testGroup "Language files"
  [ testCase "./jappie-lang/lambda.jappie" $ do
      res <- liftIO $ parseFromFile parseExpressions "test/jappie-lang/lambda.jappie"
      res @?= Just
        [ Comment " comment"
        , Comment ""
        , Comment " identity"
        , Lam (MkName "x") (Var (MkName "x"))
        , Comment " app"
        , App (Lam (MkName "x") (Var (MkName "x"))) (Lam (MkName "y") (Var (MkName "y")))
        , Comment " ski"
        , Comment " s "
        , Lam (MkName "f") (Lam (MkName "g") (Lam (MkName "x") (App (App (Var (MkName "f")) (Var (MkName "x"))) (App (Var (MkName "g")) (Var (MkName "x"))))))
        , Comment "k (aka const)"
        , Lam (MkName "x") (Lam (MkName "y") (Var (MkName "x")))
        ]
  , testCase "./jappie-lang/multi-line.jappie" $ do
      res <- liftIO $ parseFromFile parseExpressions "test/jappie-lang/multi-line.jappie"
      res @?= Just
        [ Comment " identity"
        , Lam (MkName "x") (Var (MkName "x"))
        ]
  ]
