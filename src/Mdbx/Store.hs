{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mdbx.Store (
  MdbxItemStore(..),
  fromMdbxStore,
  withMdbxStore
) where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Coerce (coerce)
import Data.Store
import Data.Store.Core
import Data.Word
import Foreign.Ptr

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSH
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Mdbx.Types

newtype MdbxItemStore a = MdbxItemStore {
  unwrapStore :: a
}

instance Store a => MdbxItem (MdbxItemStore a) where
  fromMdbxVal item = MdbxItemStore <$> fromMdbxStore item
  toMdbxVal item = withMdbxStore (unwrapStore item)

fromMdbxStore :: Store v => MdbxVal -> IO v
fromMdbxStore (MdbxVal size ptr) = do
  bs <- unsafePackCStringLen (castPtr ptr, fromIntegral size)
  decodeIO bs

withMdbxStore :: Store v => v -> (MdbxVal -> IO a) -> IO a
withMdbxStore val fn =
  unsafeUseAsCStringLen (encode val) $ \(ptrV, sizeV) -> do
    let mval = MdbxVal (fromIntegral sizeV) (castPtr ptrV)
    fn mval

instance Store NullByteString where
  size = VarSize
    $ \bs -> BS.length (BSH.fromShort $ unNullByteString bs) + 1

  poke nbs = do
    let bs = BSH.fromShort $ unNullByteString nbs
    let (sourceFp, sourceOffset, sourceLength) = BSI.toForeignPtr bs
    pokeFromForeignPtr sourceFp sourceOffset sourceLength
    poke (0 :: Word8)

  peek = Peek $ \ps ptr -> do
    bs <- BS.packCString (castPtr ptr)
    let newPtr = ptr `plusPtr` (BS.length bs + 1)
    let nbs = NullByteString (BSH.toShort bs)
    return $ PeekResult newPtr nbs

deriving via (MdbxItemStore NullByteString) instance MdbxItem NullByteString

instance Store NullText where
  size = VarSize
    $ \t -> BS.length (TE.encodeUtf8 $ unNullText t) + 1

  poke nt = do
    let bs = TE.encodeUtf8 $ unNullText nt
    let (sourceFp, sourceOffset, sourceLength) = BSI.toForeignPtr bs
    pokeFromForeignPtr sourceFp sourceOffset sourceLength
    poke (0 :: Word8)

  peek = Peek $ \ps ptr -> do
    bs <- BS.packCString (castPtr ptr)
    let newPtr = ptr `plusPtr` (BS.length bs + 1)
    let nt = NullText (TE.decodeUtf8 bs)
    return $ PeekResult newPtr nt

deriving via (MdbxItemStore NullText) instance MdbxItem NullText
