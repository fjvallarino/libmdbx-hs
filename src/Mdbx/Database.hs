{-|
Module      : Mdbx.Database
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

High level API to create, update, delete and query an MDBX database.
-}
module Mdbx.Database (
  -- * Get
  getItem,
  getItems,
  -- * Query
  getRange,
  -- * Save
  putItem,
  putItems,
  -- * Delete
  delItem,
  delItems
) where

import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (fix)
import Data.Maybe (catMaybes, fromJust)

import Mdbx.API
import Mdbx.Types

-- | Returns the value associated with the given key, if any.
getItem
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The key to lookup.
  -> m (Maybe v)   -- ^ The matching value, if any.
getItem env dbi key = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- liftIO $ toMdbxVal key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM fromMdbxVal mval
  void $ txnCommit txn
  return resp

{-|
Returns the values associated to a list of keys. Returned length may not match
that of provided keys in case some of them are not found.
-}
getItems
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> [k]           -- ^ The keys to lookup.
  -> m [v]         -- ^ The matching values. Length may not match that of keys.
getItems env dbi keys = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- forM keys $ \key ->
    liftIO $ toMdbxVal key $ \mkey -> do
      mval <- itemGet txn dbi mkey
      mapM fromMdbxVal mval
  void $ txnCommit txn
  return $ catMaybes resp

-- | Returns the list of values whose keys lie between the provided range.
getRange
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The start of the range (inclusive).
  -> k             -- ^ The end of the range (inclusive).
  -> m [v]         -- ^ The matching values.
getRange env dbi start end = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]
  cursor <- cursorOpen txn dbi

  pairs <- liftIO $ toMdbxVal start $ \skey ->
    liftIO $ toMdbxVal end $ \ekey -> do
      pair1 <- cursorRange cursor skey
      flip fix (pair1, []) $ \loop (pair, items) -> do
        isValid <- pairLEKey txn dbi ekey pair

        if isValid
          then do
            val <- fromMdbxVal . snd . fromJust $ pair
            newPair <- cursorNext cursor

            loop (newPair, val : items)
          else return items

  void $ txnCommit txn
  return $ reverse pairs

-- | Saves the given key-value pair.
putItem
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The key.
  -> v             -- ^ The value.
  -> m ()
putItem env dbi key item = do
  txn <- txnBegin env Nothing []
  liftIO $ toMdbxVal key $ \mkey ->
    liftIO $ toMdbxVal item $ \mitem ->
      itemPut txn dbi mkey mitem []
  void $ txnCommit txn

-- | Saves the given key-value pairs. Runs in a single transaction.
putItems
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> [(k, v)]      -- ^ The list of key-value pairs.
  -> m ()
putItems env dbi items = do
  txn <- txnBegin env Nothing []
  forM_ items $ \(key, item) ->
    liftIO $ toMdbxVal key $ \mkey ->
      liftIO $ toMdbxVal item $ \mitem ->
        itemPut txn dbi mkey mitem []
  void $ txnCommit txn

-- | Deletes the item associated with the given key, if any.
delItem
  :: (MonadIO m, MonadFail m, MdbxItem k)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The key to delete.
  -> m ()
delItem env dbi key = do
  txn <- txnBegin env Nothing []
  liftIO $ toMdbxVal key $ \mkey ->
    itemDel txn dbi mkey Nothing
  void $ txnCommit txn

{-|
Deletes the items associated with the given keys, if any. Runs in a single
transaction.
-}
delItems
  :: (MonadIO m, MonadFail m, MdbxItem k)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> [k]           -- ^ The keys to delete.
  -> m ()
delItems env dbi keys = do
  txn <- txnBegin env Nothing []
  forM_ keys $ \key ->
    liftIO $ toMdbxVal key $ \mkey ->
      itemDel txn dbi mkey Nothing
  void $ txnCommit txn

-- Helpers

-- | Checks if the key of the key-value pair is lower than the provided key.
pairLEKey
  :: (MonadIO m, MonadFail m)
  => MdbxTxn       -- ^ The active transaction.
  -> MdbxDbi       -- ^ The database.
  -> MdbxVal       -- ^ The reference key.
  -> Maybe (MdbxVal, MdbxVal)  -- ^ The key-value pair to check
  -> m Bool        -- ^ True if the key-value is lower or equal than the key.
pairLEKey txn dbi end Nothing = return False
pairLEKey txn dbi end (Just (key, _)) = (<= 0) <$> keyCmp txn dbi key end
