{-|
Module      : Mdbx.Database
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

High level API to create, update, delete and query an MDBX database.
-}
{-# LANGUAGE Strict #-}

module Mdbx.Database (
  -- * Get
  getItem,
  getItems,
  getRange,
  getRangePairs,
  getBounds,
  -- * Save
  putItem,
  putItems,
  -- * Delete
  delItem,
  delItems,
  delRange
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException(..), bracket, catch, throw)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Function (fix)
import Data.Maybe (catMaybes, fromJust, isJust)

import Mdbx.API
import Mdbx.Types

-- | Returns the value associated with the given key, if any.
getItem
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The key to lookup.
  -> m (Maybe v)   -- ^ The matching value, if any.
getItem env dbi key = liftIO . doInReadTxn env $ \txn -> do
  toMdbxVal key $ \mkey -> do
    mval <- itemGet txn dbi mkey
    mapM fromMdbxVal mval

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
getItems env dbi keys = liftIO . doInReadTxn env $ \txn -> do
  resp <- forM keys $ \key ->
    toMdbxVal key $ \mkey -> do
      mval <- itemGet txn dbi mkey
      mapM fromMdbxVal mval

  return $ catMaybes resp

-- | Returns the list of values whose keys lie between the provided range.
getRange
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The start of the range (inclusive).
  -> k             -- ^ The end of the range (inclusive).
  -> m [v]         -- ^ The matching values.
getRange env dbi start end = liftIO . doInReadTxn env $ \txn -> do
  doInCursor txn dbi $ \cursor ->
    toMdbxVal start $ \skey ->
      toMdbxVal end $ \ekey -> do
        pair1 <- cursorRange cursor skey
        flip fix (pair1, []) $ \loop (pair, items) -> do
          isValid <- pairLEKey txn dbi ekey pair

          if isValid
            then do
              val <- fromMdbxVal . snd . fromJust $ pair
              newPair <- cursorNext cursor

              loop (newPair, val : items)
            else return (reverse items)

{-|
Returns the list of key/value pairs whose keys lie between the provided range.
-}
getRangePairs
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The start of the range (inclusive).
  -> k             -- ^ The end of the range (inclusive).
  -> m [(k, v)]    -- ^ The matching pairs.
getRangePairs env dbi start end = liftIO . doInReadTxn env $ \txn ->
  doInCursor txn dbi $ \cursor ->
    toMdbxVal start $ \skey ->
      toMdbxVal end $ \ekey -> do
        pair1 <- cursorRange cursor skey
        flip fix (pair1, []) $ \loop (pair, items) -> do
          isValid <- pairLEKey txn dbi ekey pair

          if isValid
            then do
              key <- fromMdbxVal . fst . fromJust $ pair
              val <- fromMdbxVal . snd . fromJust $ pair

              newPair <- cursorNext cursor

              loop (newPair, (key, val) : items)
            else return (reverse items)

{-|
Returns the minimum and maximum keys, and their respective values, between the
provided key range.

Both start and end keys are inclusive, thus the same key/value pairs will be
returned if they exist. Otherwise, the next\/previous valid key\/value pairs
will be returned respectively.
-}
getBounds
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv                       -- ^ The environment.
  -> MdbxDbi                       -- ^ The database.
  -> k                             -- ^ The start of the range (inclusive).
  -> k                             -- ^ The end of the range (inclusive).
  -> m (Maybe ((k, v), (k, v)))    -- ^ The bounding pairs, if any.
getBounds env dbi start end = liftIO . doInReadTxn env $ \txn -> do
  doInCursor txn dbi $ \cursor ->
    toMdbxVal start $ \skey ->
      toMdbxVal end $ \ekey -> do
        pairMem1 <- cursorRange cursor skey
        isValid1 <- pairLEKey txn dbi ekey pairMem1
        pair1 <- fetchIfValid isValid1 pairMem1

        pairMem2 <- runMaybeT $ MaybeT (cursorAt cursor ekey) <|> MaybeT (cursorPrev cursor)
        isValid2 <- pairGEKey txn dbi skey pairMem2
        pair2 <- fetchIfValid isValid2 pairMem2

        return $ (,) <$> pair1 <*> pair2
  where
    fromMdbxPairs (mkey, mval) = (,) <$> fromMdbxVal mkey <*> fromMdbxVal mval
    fetchIfValid True pairMem = mapM fromMdbxPairs pairMem
    fetchIfValid _ _ = return Nothing

-- | Saves the given key/value pair.
putItem
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The key.
  -> v             -- ^ The value.
  -> m ()
putItem env dbi key item =
  liftIO . doInWriteTxn env $ \txn ->
    toMdbxVal key $ \mkey ->
      toMdbxVal item $ \mitem ->
        itemPut txn dbi mkey mitem []

-- | Saves the given key/value pairs. Runs in a single transaction.
putItems
  :: (MonadIO m, MonadFail m, MdbxItem k, MdbxItem v)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> [(k, v)]      -- ^ The list of key/value pairs.
  -> m ()
putItems env dbi items =
  liftIO . doInWriteTxn env $ \txn ->
    forM_ items $ \(key, item) ->
      toMdbxVal key $ \mkey ->
        toMdbxVal item $ \mitem ->
          itemPut txn dbi mkey mitem []

-- | Deletes the item associated with the given key, if any.
delItem
  :: (MonadIO m, MonadFail m, MdbxItem k)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The key to delete.
  -> m ()
delItem env dbi key =
  liftIO . doInWriteTxn env $ \txn ->
    liftIO $ toMdbxVal key $ \mkey ->
      itemDel txn dbi mkey Nothing

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
delItems env dbi keys =
  liftIO . doInWriteTxn env $ \txn ->
    forM_ keys $ \key ->
      toMdbxVal key $ \mkey ->
        itemDel txn dbi mkey Nothing

-- | Deletes the values whose keys lie between the provided range.
delRange
  :: (MonadIO m, MonadFail m, MdbxItem k)
  => MdbxEnv       -- ^ The environment.
  -> MdbxDbi       -- ^ The database.
  -> k             -- ^ The start of the range (inclusive).
  -> k             -- ^ The end of the range (inclusive).
  -> m ()
delRange env dbi start end =
  liftIO . doInWriteTxn env $ \txn -> do
    doInCursor txn dbi $ \cursor ->
      toMdbxVal start $ \skey ->
        toMdbxVal end $ \ekey -> do
          pair1 <- cursorRange cursor skey
          flip fix pair1 $ \loop pair -> do
            isValid <- pairLEKey txn dbi ekey pair

            when isValid $ do
              let mkey = fst . fromJust $ pair
              itemDel txn dbi mkey Nothing
              newPair <- cursorNext cursor

              loop newPair

-- Helpers

{-|
Checks if the key of the key/value pair is lower or equal than the provided key.
-}
pairLEKey
  :: (MonadIO m, MonadFail m)
  => MdbxTxn       -- ^ The active transaction.
  -> MdbxDbi       -- ^ The database.
  -> MdbxVal       -- ^ The reference key.
  -> Maybe (MdbxVal, MdbxVal)  -- ^ The key/value pair to check
  -> m Bool        -- ^ True if the key/value is lower or equal than the key.
pairLEKey txn dbi end Nothing = return False
pairLEKey txn dbi end (Just (key, _)) = (<= 0) <$> keyCmp txn dbi key end

{-|
Checks if the key of the key/value pair is greater or equal than the provided
key.
-}
pairGEKey
  :: (MonadIO m, MonadFail m)
  => MdbxTxn       -- ^ The active transaction.
  -> MdbxDbi       -- ^ The database.
  -> MdbxVal       -- ^ The reference key.
  -> Maybe (MdbxVal, MdbxVal)  -- ^ The key/value pair to check
  -> m Bool        -- ^ True if the key/value is greater or equal than the key.
pairGEKey txn dbi end Nothing = return False
pairGEKey txn dbi end (Just (key, _)) = (>= 0) <$> keyCmp txn dbi key end

-- | Runs the given action in a read only transaction.
doInReadTxn
  :: MdbxEnv
  -> (MdbxTxn -> IO a)
  -> IO a
doInReadTxn env action = doInTxn env Nothing [MdbxTxnRdonly] action

{-|
Runs the given action in a read/write transaction, that will be aborted if an
exception is thrown and committed otherwise.
-}
doInWriteTxn
  :: MdbxEnv
  -> (MdbxTxn -> IO a)
  -> IO a
doInWriteTxn env action = doInTxn env Nothing [] action

{-|
Runs the given action in a transaction with the specified flags and, optionally,
parent transaction. The transaction will be aborted if an exception is thrown,
committed otherwise.
-}
doInTxn
  :: MdbxEnv
  -> Maybe MdbxTxn
  -> [MdbxTxnFlags]
  -> (MdbxTxn -> IO a)
  -> IO a
doInTxn env parentTxn flags action = do
  txn <- txnBegin env parentTxn flags
  handleAny (handleEx txn) $ do
    res <- action txn
    txnCommit txn
    return res
  where
    handleEx txn e = txnAbort txn >> throw e

{-|
Runs the given action with an open cursor which will be closed when finished or
on error.
-}
doInCursor
  :: MdbxTxn
  -> MdbxDbi
  -> (MdbxCursor -> IO a)
  -> IO a
doInCursor txn dbi action = bracket (cursorOpen txn dbi) cursorClose action

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny action handler = catch handler action
