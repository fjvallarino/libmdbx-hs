module MDBX.Database (
  getItem,
  getItems,
  getRange,
  putItem,
  putItems,
  delItem,
  delItems
) where

import Control.Monad (forM, forM_, void)
import Data.Function (fix)
import Data.Maybe (catMaybes, fromJust)

import MDBX.API
import MDBX.Types

getItem :: (MdbxKey k, MdbxItem v) => MdbxEnv -> MdbxDbi -> k -> IO (Maybe v)
getItem env dbi key = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- withMdbxKey key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM fromMdbxItem mval
  void $ txnCommit txn
  return resp

getItems :: (MdbxKey k, MdbxItem v) => MdbxEnv -> MdbxDbi -> [k] -> IO [v]
getItems env dbi keys = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- forM keys $ \key -> withMdbxKey key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM fromMdbxItem mval
  void $ txnCommit txn
  return $ catMaybes resp

getRange :: (MdbxKey k, MdbxItem v) => MdbxEnv -> MdbxDbi -> k -> k -> IO [v]
getRange env dbi start end = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]
  cursor <- cursorOpen txn dbi

  pairs <- withMdbxKey start $ \skey ->
    withMdbxKey end $ \ekey -> do
      pair1 <- cursorRange cursor skey
      flip fix (pair1, []) $ \loop (pair, items) -> do
        isValid <- pairLEKey txn dbi ekey pair

        if isValid
          then do
            val <- fromMdbxItem . snd . fromJust $ pair
            newPair <- cursorNext cursor

            loop (newPair, val : items)
          else return items

  void $ txnCommit txn
  return $ reverse pairs

putItem :: (MdbxKey k, MdbxItem v) => MdbxEnv -> MdbxDbi -> k -> v -> IO ()
putItem env dbi key item = do
  txn <- txnBegin env Nothing []
  withMdbxKey key $ \mkey ->
    withMdbxItem item $ \mitem ->
      itemPut txn dbi mkey mitem []
  void $ txnCommit txn

putItems :: (MdbxKey k, MdbxItem v) => MdbxEnv -> MdbxDbi -> [(k, v)] -> IO ()
putItems env dbi items = do
  txn <- txnBegin env Nothing []
  forM_ items $ \(key, item) ->
    withMdbxKey key $ \mkey ->
      withMdbxItem item $ \mitem ->
        itemPut txn dbi mkey mitem []
  void $ txnCommit txn

delItem :: MdbxKey k => MdbxEnv -> MdbxDbi -> k -> IO ()
delItem env dbi key = do
  txn <- txnBegin env Nothing []
  withMdbxKey key $ \mkey ->
    itemDel txn dbi mkey Nothing
  void $ txnCommit txn

delItems :: MdbxKey k => MdbxEnv -> MdbxDbi -> [k] -> IO ()
delItems env dbi keys = do
  txn <- txnBegin env Nothing []
  forM_ keys $ \key ->
    withMdbxKey key $ \mkey ->
      itemDel txn dbi mkey Nothing
  void $ txnCommit txn

-- Helpers
pairLEKey :: MdbxTxn -> MdbxDbi -> MdbxVal -> Maybe (MdbxVal, MdbxVal) -> IO Bool
pairLEKey txn dbi end Nothing = return False
pairLEKey txn dbi end (Just (key, _)) = (<= 0) <$> keyCmp txn dbi key end
