import Test.Hspec
import Test.QuickCheck

import LogAnalysis
import Log

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LogAnalysis.parseMessage" $ do
    context "Unknown" $ do
      it "empty pessage" $ do
        parseMessage "" `shouldBe` Unknown ""

      it "without flag" $ do
        parseMessage "P 12 message" `shouldBe` Unknown "P 12 message"

    context "Info" $ do
      it "LogMessage" $ do
        parseMessage "I 23 help me" `shouldBe` LogMessage Info 23 "help me"

    context "Warning" $ do
      it "LogMessage" $ do
        parseMessage "W 23 help me" `shouldBe` LogMessage Warning 23 "help me"

    context "Error" $ do
      it "LogMessage" $ do
        parseMessage "E 100 23 help me" `shouldBe` LogMessage (Error 100) 23 "help me"

  describe "LogAnalysis.parse" $ do
    context "success" $ do
      it "read single line and return a array with one LogMessage" $ do
        testParse parse 1 "test/error.log" `shouldReturn` [LogMessage Info 5053 "pci_id: con ing!"]
