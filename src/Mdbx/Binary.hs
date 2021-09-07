{-|
Module      : Mdbx.Binary
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Instances and helpers to derive 'MdbxItem' for 'Binary' instances.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mdbx.Binary (
  MdbxItemBinary(..)
) where

import Control.Monad (forM_)
import Data.Binary
import Data.Binary.Get (getLazyByteStringNul)
import Foreign.Ptr (castPtr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSH
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as TE

import Mdbx.Types

{-|
Helper type to derive 'MdbxItem' instances for types implementing 'Binary' using
the newtype deriving trick.
-}
newtype MdbxItemBinary a = MdbxItemBinary {
  unwrapBinary :: a
}

instance Binary a => MdbxItem (MdbxItemBinary a) where
  fromMdbxVal item = MdbxItemBinary <$> fromMdbxBinary item
  toMdbxVal item = withMdbxBinary (unwrapBinary item)

-- | Deserializes a 'Binary' instance from an 'MdbxVal'.
fromMdbxBinary :: Binary v => MdbxVal -> IO v
fromMdbxBinary (MdbxVal size ptr) = do
  bs <- BSU.unsafePackCStringLen (castPtr ptr, fromIntegral size)
  return $ decode (BSL.fromStrict bs)

-- | Serializes a 'Binary' instance to 'MdbxVal', and passes it to a callback.
withMdbxBinary :: Binary v => v -> (MdbxVal -> IO a) -> IO a
withMdbxBinary val fn =
  BSU.unsafeUseAsCStringLen (BSL.toStrict (encode val)) $ \(ptrV, sizeV) -> do
    let mval = MdbxVal (fromIntegral sizeV) (castPtr ptrV)
    fn mval

instance Binary NullByteString where
  put nbs = do
    forM_ (BSH.unpack $ unNullByteString nbs) putWord8
    putWord8 0

  get = NullByteString . BSH.toShort . BSL.toStrict <$> getLazyByteStringNul

deriving via (MdbxItemBinary NullByteString) instance MdbxItem NullByteString

instance Binary NullText where
  put nbs = do
    forM_ (BS.unpack . TE.encodeUtf8 $ unNullText nbs) putWord8
    putWord8 0

  get = NullText . TE.decodeUtf8 . BSL.toStrict <$> getLazyByteStringNul

deriving via (MdbxItemBinary NullText) instance MdbxItem NullText
