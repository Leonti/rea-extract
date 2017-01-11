module PriceParsingSpec where

import Test.Hspec

import PriceParsing

spec :: Spec
spec =
  describe "parsePrice" $ do
    it "parses simple price" $
        parsePrice "$390,000" `shouldBe` Just 390000
    it "parses simple price with spaces" $
        parsePrice " $390,000 " `shouldBe` Just 390000
    it "parses range" $
        parsePrice "$450,000 - $490,000" `shouldBe` Just 490000
    it "parses price with text" $
        parsePrice "PRIVATE SALE $429,000" `shouldBe` Just 429000
    it "parses price with +" $
        parsePrice "$280,000+" `shouldBe` Just 280000
    it "parses price `K`" $
        parsePrice "GREAT INVESTMENT $325K" `shouldBe` Just 325000
    it "parses price with text and +" $
        parsePrice "Private Sale $280,000+" `shouldBe` Just 280000
    it "parses price with text after price" $
        parsePrice "$490,000 Plus" `shouldBe` Just 490000        
