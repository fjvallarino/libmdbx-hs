module Mdbx.ConversionSpec (spec) where

import Test.Hspec

import Mdbx

spec :: Spec
spec = describe "Number conversion" $ do
  it "should convert an Int to Word32" $ do
    keyFromInt32 (-100) `shouldBe` 2147483548
    keyFromInt32      0 `shouldBe` 2147483648
    keyFromInt32    100 `shouldBe` 2147483748

  it "should convert a Word32 to Int" $ do
    int32FromKey  2147483548 `shouldBe` -100
    int32FromKey  2147483648 `shouldBe`    0
    int32FromKey  2147483748 `shouldBe`  100

  it "should convert an Int to Word64" $ do
    keyFromInt64 (-100) `shouldBe` 9223372036854775708
    keyFromInt64      0 `shouldBe` 9223372036854775808
    keyFromInt64    100 `shouldBe` 9223372036854775908

  it "should convert a Word64 to Int" $ do
    int64FromKey 9223372036854775708 `shouldBe` -100
    int64FromKey 9223372036854775808 `shouldBe`    0
    int64FromKey 9223372036854775908 `shouldBe`  100

  it "should convert a Float to Word32" $ do
    keyFromFloat (-100) `shouldBe` 1027080191
    keyFromFloat      0 `shouldBe` 2147483648
    keyFromFloat    100 `shouldBe` 3267887104

  it "should convert a Word32 to Float" $ do
    floatFromKey 1027080191 `shouldBe` -100
    floatFromKey 2147483648 `shouldBe`    0
    floatFromKey 3267887104 `shouldBe`  100

  it "should convert a Double to Word64" $ do
    keyFromDouble (-100) `shouldBe`  4586634745500139519
    keyFromDouble      0 `shouldBe`  9223372036854775808
    keyFromDouble    100 `shouldBe` 13860109328209412096

  it "should convert a Word64 to Double" $ do
    doubleFromKey  4586634745500139519 `shouldBe` -100
    doubleFromKey  9223372036854775808 `shouldBe`    0
    doubleFromKey 13860109328209412096 `shouldBe`  100
