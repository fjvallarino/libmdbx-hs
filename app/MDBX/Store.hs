module MDBX.Store (
  MdbxItemStore(..),
  fromMdbxStore,
  withMdbxStore
) where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Coerce (coerce)
import Data.Store (Store, decodeIO, encode)
import Foreign.Ptr

import MDBX

newtype MdbxItemStore a = MdbxItemStore {
  unwrapStore :: a
}

instance Store a => MdbxItem (MdbxItemStore a) where
  fromMdbxItem item = MdbxItemStore <$> fromMdbxStore item
  withMdbxItem item = withMdbxStore (unwrapStore item)

fromMdbxStore :: Store v => MdbxVal -> IO v
fromMdbxStore (MdbxVal size ptr) = do
  bs <- unsafePackCStringLen (castPtr ptr, fromIntegral size)
  decodeIO bs

withMdbxStore :: Store v => v -> (MdbxVal -> IO a) -> IO a
withMdbxStore val fn =
  unsafeUseAsCStringLen bsV $ \(ptrV, sizeV) -> do
    let mval = MdbxVal (fromIntegral sizeV) (castPtr ptrV)
    fn mval
  where
    bsV = encode val
