module Store (
  getItem,
  getItems,
  getItemsRange,
  putItem,
  putItems,
  delItem,
  delItems
) where

import Control.Monad (forM, forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Function (fix)
import Data.Maybe
import Data.Store (Store, decodeIO, encode)
import Data.Traversable (sequenceA)
import Foreign.Ptr

import MDBX

getItem :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> k -> IO (Maybe v)
getItem env dbi key = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- withKey key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM fromMVal mval
  void $ txnCommit txn
  return resp

getItems :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> [k] -> IO [v]
getItems env dbi keys = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- forM keys $ \key -> withKey key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM fromMVal mval
  void $ txnCommit txn
  return $ catMaybes resp

getItemsRange :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> k -> k -> IO [(k, v)]
getItemsRange env dbi start end = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]
  cursor <- cursorOpen txn dbi

  pairs <- withPair start end $ \skey ekey -> do
    pair1 <- cursorRange cursor skey
    flip fix (pair1, []) $ \loop (pair, items) -> do
      isValid <- validMKey txn dbi ekey pair

      if isValid
        then do
          key <- fromMVal . fst . fromJust $ pair
          val <- fromMVal . snd . fromJust $ pair
          newPair <- cursorNext cursor

          loop (newPair, (key, val) : items)
        else return items

  void $ txnCommit txn
  return pairs

validMKey :: MdbxTxn -> MdbxDbi -> MdbxVal -> Maybe (MdbxVal, MdbxVal) -> IO Bool
validMKey txn dbi end Nothing = return False
validMKey txn dbi end (Just (key, _)) = (<= 0) <$> keyCmp txn dbi key end

putItem :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> k -> v -> IO ()
putItem env dbi key val = do
  txn <- txnBegin env Nothing []
  withPair key val $ \mkey mval ->
    itemPut txn dbi mkey mval []
  void $ txnCommit txn

putItems :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> [(k, v)] -> IO ()
putItems env dbi items = do
  txn <- txnBegin env Nothing []
  forM_ items $ \(key, val) -> withPair key val $ \mkey mval ->
    itemPut txn dbi mkey mval []
  void $ txnCommit txn

delItem :: Store k => MdbxEnv -> MdbxDbi -> k -> IO ()
delItem env dbi key = do
  txn <- txnBegin env Nothing []
  withKey key $ \mkey ->
    itemDel txn dbi mkey Nothing
  void $ txnCommit txn

delItems :: Store k => MdbxEnv -> MdbxDbi -> [k] -> IO ()
delItems env dbi keys = do
  txn <- txnBegin env Nothing []
  forM_ keys $ \key -> withKey key $ \mkey ->
    itemDel txn dbi mkey Nothing
  void $ txnCommit txn

withKey :: Store v => v -> (MdbxVal -> IO a) -> IO a
withKey val fn = unsafeUseAsCStringLen bs $ \(ptr, size) ->
  fn $ MdbxVal (fromIntegral size) (castPtr ptr)
  where
    bs = encode val

withPair :: (Store k, Store v) => k -> v -> (MdbxVal -> MdbxVal -> IO a) -> IO a
withPair key val fn =
  unsafeUseAsCStringLen bsK $ \(ptrK, sizeK) ->
    unsafeUseAsCStringLen bsV $ \(ptrV, sizeV) -> do
      let mkey = MdbxVal (fromIntegral sizeK) (castPtr ptrK)
      let mval = MdbxVal (fromIntegral sizeV) (castPtr ptrV)
      fn mkey mval
  where
    bsK = encode key
    bsV = encode val

fromMVal :: Store v => MdbxVal -> IO v
fromMVal (MdbxVal size ptr) = do
  bs <- unsafePackCStringLen (castPtr ptr, fromIntegral size)
  decodeIO bs
