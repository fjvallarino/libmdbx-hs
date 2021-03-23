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
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Traversable (sequenceA)
import Foreign.Ptr

import MDBX
import Foreign.Storable (sizeOf)

getItem :: Store v => MdbxEnv -> MdbxDbi -> Text -> IO (Maybe v)
getItem env dbi key = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- withKey key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM toVal mval
  void $ txnCommit txn
  return resp

getItems :: Store v => MdbxEnv -> MdbxDbi -> [Text] -> IO [v]
getItems env dbi keys = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- forM keys $ \key -> withKey key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM toVal mval
  void $ txnCommit txn
  return $ catMaybes resp

getItemsRange :: Store v => MdbxEnv -> MdbxDbi -> Text -> Text -> IO [(Text, v)]
getItemsRange env dbi start end = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]
  cursor <- cursorOpen txn dbi

  pairs <- withKey start $ \skey ->
    withKey end $ \ekey -> do
      pair1 <- cursorRange cursor skey
      flip fix (pair1, []) $ \loop (pair, items) -> do
        isValid <- validMKey txn dbi ekey pair

        if isValid
          then do
            key <- toKey . fst . fromJust $ pair
            val <- toVal . snd . fromJust $ pair
            newPair <- cursorNext cursor

            loop (newPair, (key, val) : items)
          else return items

  void $ txnCommit txn
  return $ reverse pairs

validMKey :: MdbxTxn -> MdbxDbi -> MdbxVal -> Maybe (MdbxVal, MdbxVal) -> IO Bool
validMKey txn dbi end Nothing = return False
validMKey txn dbi end (Just (key, _)) = (<= 0) <$> keyCmp txn dbi key end

putItem :: Store v => MdbxEnv -> MdbxDbi -> Text -> v -> IO ()
putItem env dbi key val = do
  txn <- txnBegin env Nothing []
  withPair key val $ \mkey mval ->
    itemPut txn dbi mkey mval []
  void $ txnCommit txn

putItems :: Store v => MdbxEnv -> MdbxDbi -> [(Text, v)] -> IO ()
putItems env dbi items = do
  txn <- txnBegin env Nothing []
  forM_ items $ \(key, val) -> withPair key val $ \mkey mval ->
    itemPut txn dbi mkey mval []
  void $ txnCommit txn

delItem :: MdbxEnv -> MdbxDbi -> Text -> IO ()
delItem env dbi key = do
  txn <- txnBegin env Nothing []
  withKey key $ \mkey ->
    itemDel txn dbi mkey Nothing
  void $ txnCommit txn

delItems :: MdbxEnv -> MdbxDbi -> [Text] -> IO ()
delItems env dbi keys = do
  txn <- txnBegin env Nothing []
  forM_ keys $ \key -> withKey key $ \mkey ->
    itemDel txn dbi mkey Nothing
  void $ txnCommit txn

withKey :: Text -> (MdbxVal -> IO a) -> IO a
withKey key fn = useAsPtr key $ \ptr size ->
  fn $ MdbxVal (fromIntegral size * 2) (castPtr ptr)

withPair :: Store v => Text -> v -> (MdbxVal -> MdbxVal -> IO a) -> IO a
withPair key val fn =
  withKey key $ \mkey ->
    unsafeUseAsCStringLen bsV $ \(ptrV, sizeV) -> do
      let mval = MdbxVal (fromIntegral sizeV) (castPtr ptrV)
      fn mkey mval
  where
    bsV = encode val

toKey :: MdbxVal -> IO Text
toKey (MdbxVal size ptr) = fromPtr (castPtr ptr) (fromIntegral size `div` 2)

toVal :: Store v => MdbxVal -> IO v
toVal (MdbxVal size ptr) = do
  bs <- unsafePackCStringLen (castPtr ptr, fromIntegral size)
  decodeIO bs
