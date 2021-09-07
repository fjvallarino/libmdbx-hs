{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Mdbx.DatabaseSpec (spec) where

import Data.Binary
import Data.Default
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Mdbx
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import TestUtil

data TestKey = TestKey {
  keyCategory :: NullByteString,
  keyGroup :: Word16,
  keyTs :: Word64
} deriving (Eq, Show, Generic, Binary)

deriving via (MdbxItemBinary TestKey) instance MdbxItem TestKey

data TestKey2 = TestKey2 {
  key2Category :: NullText,
  key2Group :: Word16,
  key2Ts :: Word64
} deriving (Eq, Show, Generic, Binary)

deriving via (MdbxItemBinary TestKey2) instance MdbxItem TestKey2

spec :: Spec
spec = do
  nullByteStringSpec
  nullText
  storeSpec

nullByteStringSpec :: Spec
nullByteStringSpec = describe "NullByteString" $ do
  it "should encode/decode NullByteString correctly" $ \env -> do
    let nbs = NullByteString "Test ByteString"

    decode (encode nbs) `shouldBe` nbs

  it "should correctly encode a key containing NullByteString" $ \env -> do
    let key1 = TestKey "Key 1" 100 1
    let key2 = TestKey "Long key 2" 100 2
    let key3 = TestKey "Longer key 3" 100 3

    BSL.length (encode key1) `shouldBe` 16
    BSL.length (encode key2) `shouldBe` 21
    BSL.length (encode key3) `shouldBe` 23

    decode (encode key1) `shouldBe` key1
    decode (encode key2) `shouldBe` key2
    decode (encode key3) `shouldBe` key3

nullText :: Spec
nullText = describe "NullText" $ do
  it "should encode/decode NullText correctly" $ \env -> do
    let nbs = NullText "Test Text - Русский"

    decode (encode nbs) `shouldBe` nbs

  it "should correctly encode a key containing NullText" $ \env -> do
    let key1 = TestKey2 "Key 1" 100 1
    let key2 = TestKey2 "Long key 2" 100 2
    let key3 = TestKey2 "Longer key 3 - Русский" 100 3

    BSL.length (encode key1) `shouldBe` 16
    BSL.length (encode key2) `shouldBe` 21
    BSL.length (encode key3) `shouldBe` 40

    decode (encode key1) `shouldBe` key1
    decode (encode key2) `shouldBe` key2
    decode (encode key3) `shouldBe` key3

storeSpec :: Spec
storeSpec = around withDatabase $
  describe "Database" $ do
    it "should insert and retrieve text keys" $ \(env, db) -> do
      let key k = k :: Text
      let val v = v :: Text

      putItem env db (key "Key 1") (val "Value 1")
      putItem env db (key "Key 22") (val "Value 2")
      putItem env db (key "Key 3") (val "Value 3")
      putItem env db (key "Key 4") (val "Value 4")
      putItem env db (key "Key 5") (val "Value 5")
      getItem env db (key "Key 1") `shouldReturn` Just (val "Value 1")
      getItem env db (key "Key 22") `shouldReturn` Just (val "Value 2")
      getRange env db (key "Key 22") (key "Key 4") `shouldReturn` [val "Value 2", val "Value 3", val "Value 4"]

    it "should insert and retrieve individual items" $ \(env, db) -> do
      let key ts = TestKey "Test" 1 ts

      putItem env db (key 1) ("Value 1" :: Text)
      putItem env db (key 2) ("Value 2" :: Text)
      putItem env db (key 3) ("Value 3" :: Text)

      getItem env db (key 1) `shouldReturn` Just @Text "Value 1"
      getItem env db (key 2) `shouldReturn` Just @Text "Value 2"
      getItem env db (key 3) `shouldReturn` Just @Text "Value 3"

    it "should insert and retrieve a list of items" $ \(env, db) -> do
      let key ts = TestKey "Test" 1 ts
      let key1 = key 1
      let key2 = key 2
      let key3 = key 3

      putItem env db key1 ("Value 1" :: Text)
      putItem env db key2 ("Value 2" :: Text)
      putItem env db key3 ("Value 3" :: Text)

      getItems env db [key1] `shouldReturn` (["Value 1"] :: [Text])
      getItems env db [key1, key2] `shouldReturn` (["Value 1", "Value 2"] :: [Text])
      getItems env db [key2, key3] `shouldReturn` (["Value 2", "Value 3"] :: [Text])
      getItems env db [key1, key2, key3] `shouldReturn` (["Value 1", "Value 2", "Value 3"] :: [Text])

    it "should insert and retrieve a range of items using all fields" $ \(env, db) -> do
      let keyA gr ts = TestKey "Category A" gr ts
      let keyB gr ts = TestKey "Category AB" gr ts
      let keyC gr ts = TestKey "Категория с" gr ts
      let ts1 = 1608859260000
      let ts2 = 1608883260000
      let ts3 = 1608918900000

      putItem env db (keyA 1 ts1) ("Value A 1 1" :: Text)
      putItem env db (keyA 1 ts2) ("Value A 1 2" :: Text)
      putItem env db (keyA 1 ts3) ("Value A 1 3" :: Text)

      putItem env db (keyA 2 ts1) ("Value A 2 1" :: Text)
      putItem env db (keyA 2 ts2) ("Value A 2 2" :: Text)
      putItem env db (keyA 2 ts3) ("Value A 2 3" :: Text)

      putItem env db (keyB 1 ts1) ("Value B 1 1" :: Text)
      putItem env db (keyB 1 ts2) ("Value B 1 2" :: Text)
      putItem env db (keyB 1 ts3) ("Value B 1 3" :: Text)

      putItem env db (keyC 1 ts1) ("Value C 1 1" :: Text)
      putItem env db (keyC 1 ts2) ("Value C 1 2" :: Text)
      putItem env db (keyC 1 ts3) ("Value C 1 3" :: Text)

      getRange env db (keyA 1 ts3) (keyA 1 ts1) `shouldReturn` ([] :: [Text])
      getRange env db (keyA 1 ts1) (keyA 1 ts3) `shouldReturn` (["Value A 1 1", "Value A 1 2", "Value A 1 3"] :: [Text])
      getRange env db (keyA 2 ts1) (keyA 2 ts3) `shouldReturn` (["Value A 2 1", "Value A 2 2", "Value A 2 3"] :: [Text])

      getRange env db (keyB 1 ts1) (keyB 1 ts3) `shouldReturn` (["Value B 1 1", "Value B 1 2", "Value B 1 3"] :: [Text])
      getRange env db (keyC 1 ts1) (keyC 1 ts3) `shouldReturn` (["Value C 1 1", "Value C 1 2", "Value C 1 3"] :: [Text])

      getRange env db (keyA 2 ts3) (keyB 1 ts2) `shouldReturn` (["Value A 2 3", "Value B 1 1", "Value B 1 2"] :: [Text])

    it "should remove a range of keys" $ \(env, db) -> do
      let key ts = TestKey "Test" 1 ts

      putItem env db (key 1) ("Value 1" :: Text)
      putItem env db (key 2) ("Value 2" :: Text)
      putItem env db (key 3) ("Value 3" :: Text)
      putItem env db (key 4) ("Value 4" :: Text)
      putItem env db (key 5) ("Value 5" :: Text)

      getRange env db (key 1) (key 5) `shouldReturn` (["Value 1", "Value 2", "Value 3", "Value 4", "Value 5"] :: [Text])

      delRange env db (key 2) (key 4)
      getRange env db (key 1) (key 5) `shouldReturn` (["Value 1", "Value 5"] :: [Text])
