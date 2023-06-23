import Test.Tasty
import Test.Tasty.HUnit
import JappieLang.Parser
import Text.Trifecta
import Control.Monad.IO.Class
import qualified JappieLang.SyntaxTree.Parsed as Parsed
import qualified JappieLang.SyntaxTree.Core as Core
import JappieLang.Eval
import TestOrphans()

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
  [testCase "App free variable y to x" $
    eval (Core.App (Core.Lam "x" (Core.var "x")) (Core.Lam "y" (Core.var "y"))) @?= Right (Core.Lam "y" (Core.var "y"))
  ]
  , testCase "lambda " $  eval (Core.Lam "x" (Core.var "x")) @?= Right (Core.Lam "x" (Core.var "x"))
  , testCase "var " $ eval (Core.var "x") @?= Right (Core.var "x")
  ]


parserUnit :: TestTree
parserUnit =  testGroup "unit" [
    testCase "parse a lambda" $
       toSuccess (parseString parseExpression mempty "([x] x)") @?= Parsed.Lam "x" (Parsed.var "x")
  , testCase "parse a var" $
       toSuccess (parseString parseExpression mempty "x") @?= Parsed.var "x"
  , testCase "parse an app" $
       toSuccess (parseString parseExpression mempty "(([x] x) ([y] y))") @?= Parsed.App (Parsed.Lam "x" (Parsed.var "x")) (Parsed.Lam "y" (Parsed.var "y"))
  , testCase "parse a comment xxxx" $
       toSuccess (parseString parseExpression mempty "; xxxx") @?= Parsed.Comment " xxxx"
  , testCase "parse a comment with lam" $
       toSuccess (parseString parseExpression mempty "; xxxx ([x] x)") @?= Parsed.Comment " xxxx ([x] x)"
  , testCase "parse a comment empty" $
       toSuccess (parseString parseExpression mempty ";") @?= Parsed.Comment ""
  ]


langFiles :: TestTree
langFiles = testGroup "Language files"
  [ testGroup "Parse expressions, eg the direct trifecta interface (old)"
  [ testCase "./jappie-lang/lambda.jappie" $ do
      res <- liftIO $ parseFromFile parseExpressions "test/jappie-lang/lambda.jappie"
      res @?= Just
        [ Parsed.Comment " comment"
        , Parsed.Comment ""
        , Parsed.Comment " identity"
        , Parsed.Lam "x" (Parsed.Var "x")
        , Parsed.Comment " app"
        , Parsed.App (Parsed.Lam "x" (Parsed.Var "x")) (Parsed.Lam "y" (Parsed.Var "y"))
        , Parsed.Comment " ski"
        , Parsed.Comment " s "
        , Parsed.Lam "f" (Parsed.Lam "g" (Parsed.Lam "x" (Parsed.App (Parsed.App (Parsed.Var "f") (Parsed.Var "x")) (Parsed.App (Parsed.Var "g") (Parsed.Var "x")))))
        , Parsed.Comment "k (aka const)"
        , Parsed.Lam "x" (Parsed.Lam "y" (Parsed.Var "x"))
        ]
  , testCase "./jappie-lang/multi-line.jappie" $ do
      res <- liftIO $ parseFromFile parseExpressions "test/jappie-lang/multi-line.jappie"
      res @?= Just
        [ Parsed.Comment " identity"
        , Parsed.Lam "x" (Parsed.var "x")
        ]
  ], testGroup "Parse file"
  [ testCase "./jappie-lang/lambda.jappie" $ do
      res <- liftIO $ parseFile "test/jappie-lang/lambda.jappie"
      res @?= Success
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
        (Parsed.App
          (Parsed.Comment "")
          (Parsed.Comment " comment"))
          (Parsed.Comment ""))
        ( Parsed.Comment " identity"))
        ( Parsed.Lam "x" (Parsed.Var "x")))
        (Parsed.Comment " app"))
        (Parsed.App (Parsed.Lam "x" (Parsed.Var "x")) (Parsed.Lam "y" (Parsed.Var "y"))))
        (Parsed.Comment " ski"))
        (Parsed.Comment " s "))
        (Parsed.Lam "f" (Parsed.Lam "g" (Parsed.Lam "x" (Parsed.App (Parsed.App (Parsed.Var "f") (Parsed.Var "x")) (Parsed.App (Parsed.Var "g") (Parsed.Var "x")))))))
        (Parsed.Comment "k (aka const)"))
        (Parsed.Lam "x" (Parsed.Lam "y" (Parsed.Var "x"))))
  , testCase "./jappie-lang/multi-line.jappie" $ do
      res <- liftIO $ parseFile "test/jappie-lang/multi-line.jappie"
      res @?= Success
        (Parsed.App (Parsed.App mempty (Parsed.Comment " identity"))
        (Parsed.Lam "x" (Parsed.var "x")))
  ]
  ]
