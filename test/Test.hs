import Test.Tasty
import Test.Tasty.HUnit
import JappieLang.Parser
import Text.Trifecta
import Control.Monad.IO.Class
import qualified JappieLang.SyntaxTree.Parsed as Parsed
import qualified JappieLang.SyntaxTree.Core as Core
import JappieLang.SyntaxTree.Core(CoreExpression)
import JappieLang.Eval
import TestOrphans()
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import JappieLang.Simplify
import JappieLang.Print
import Data.Text.Lazy(toStrict)
import Test.Tasty.Hedgehog
import JappieLang.SyntaxTree.Name
import qualified Hedgehog.Range as Range
import Data.Text(pack)
import Test.Golden

main :: IO ()
main = do
  golden <- goldenTests
  defaultMain $ testGroup "all tests" [ unitTests,  golden]

genName :: Hedgehog.Gen Name
genName = MkName <$> Gen.text (Range.constant 1 20) Gen.alpha

generateCore :: Hedgehog.Gen CoreExpression
generateCore = generateCore' 0

generateCore' :: Int -> Hedgehog.Gen CoreExpression
generateCore' x = Gen.frequency [(1, Core.Var <$> genName),
                          (max 0 (4 - x), Core.App <$> generateCore' (x + 1) <*> generateCore' (x + 1)),
                          (max 0 (3 - x), Core.Lam <$> genName <*> generateCore' (x + 1))
                         ]

unitTests :: TestTree
unitTests = testGroup "unit tests" [
  testGroup "printer" [
      testProperty "roundTripParse " $ Hedgehog.property $ do
          xx <- Hedgehog.forAll generateCore
          Hedgehog.tripping xx (toStrict . printCoreExpression) (fmap (either (Core.var . pack . show) id . simplify) . parseText)


      , testCase "commutativity" $ let
           expression = Core.App (Core.var "a") (Core.App (Core.var "a") (Core.var "a"))
          in
            (fmap simplify $ parseText $ toStrict $ printCoreExpression expression)
            @?= (Success (Right expression))

  ],
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
  , testCase "apply name" $ eval (Core.App (Core.var "x") (Core.var "y")) @?= Left (ApplyingNameTo "x" (Core.var "y"))
  ]

parserUnit :: TestTree
parserUnit =  testGroup "unit" [
    testCase "parse a lambda" $
       (parseString parseExpression mempty "([x] x)") @?= Success (Parsed.Lam "x" (Parsed.var "x"))
  , testCase "parse a var" $
       (parseString parseExpression mempty "x") @?= Success (Parsed.var "x")
  , testCase "parse an app" $
       (parseString parseExpression mempty "(([x] x) ([y] y))") @?= Success (Parsed.App (Parsed.Lam "x" (Parsed.var "x")) (Parsed.Lam "y" (Parsed.var "y")))
  , testCase "parse a comment xxxx" $
       (parseString parseExpression mempty "; xxxx") @?= Success (Parsed.Comment " xxxx")
  , testCase "parse a comment with lam" $
       (parseString parseExpression mempty "; xxxx ([x] x)") @?= Success (Parsed.Comment " xxxx ([x] x)")
  , testCase "parse a comment empty" $
       (parseString parseExpression mempty ";") @?= Success (Parsed.Comment "")
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
