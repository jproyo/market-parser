module Main (main) where

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "main" $ do

    it "should responds with hello world" $
      "hello world" `shouldBe` "hello world"
