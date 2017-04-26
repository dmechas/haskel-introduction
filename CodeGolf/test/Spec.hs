import Test.Hspec
import Test.QuickCheck

import CodeGolf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CodeGolf" $ do
    -- it "empty list" $ do
    --     skips [] `shouldBe` []

    it "String" $ do
        skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

    it "Other String" $ do
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

    it "Int Array with 1 element" $ do
        skips [1] `shouldBe` [[1]]

    it "Boolean Array" $ do
        skips [True, False] `shouldBe` [[True, False], [False]]