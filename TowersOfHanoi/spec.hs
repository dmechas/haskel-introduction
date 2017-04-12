import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Hanoi

main :: IO ()
main = hspec $ do
  describe "Hanoi" $ do
    it "must have 1 move if 1 disk" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a","b")]

    it "tutorial example" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"),("a","b"),("c","b")]

    it "asd" $ property $
      \x -> x + x `shouldBe` (x :: Int) * 3


