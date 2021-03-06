{-|
Module      : Mdbx.API
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Thin wrappers over the low level API to provide MonadIO support and exception
based error handling.
-}
{-# LANGUAGE FlexibleContexts #-}

module Mdbx.API (
  -- * Keys
  keyCmp,
  -- * Environment
  envOpen,
  envClose,
  -- * Transaction
  txnBegin,
  txnCommit,
  txnAbort,
  -- * Database
  dbiOpen,
  dbiClose,
  -- * Data manipulation
  itemPut,
  itemGet,
  itemDel,
  -- * Cursors
  cursorOpen,
  cursorClose,
  cursorPut,
  cursorDel,
  cursorFirst,
  cursorLast,
  cursorAt,
  cursorRange,
  cursorNext,
  cursorPrev,
  cursorMove,

  -- * Conversion for types used in keys
  keyFromFloat,
  keyFromDouble,
  keyFromInt32,
  keyFromInt64,
  floatFromKey,
  doubleFromKey,
  int32FromKey,
  int64FromKey
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail)
import Data.Int
import Data.Word

import Mdbx.FFI
import Mdbx.Types

{-|
Compares two keys and returns -1, 0 or 1 if key1 is lower, equal or greater than
key2.
-}
keyCmp
  :: MonadIO m
  => MdbxTxn
  -> MdbxDbi
  -> MdbxVal
  -> MdbxVal
  -> m Int
keyCmp txn dbi key1 key2 = liftIO $ mdbx_cmp txn dbi key1 key2

{-|
Opens an environment.

When using the multi-threaded runtime, the `MdbxNotls` flag is required at
environment creation. Failing to include it in the list of flags will result in
a random crash.
-}
envOpen
  :: (MonadIO m, MonadFail m)
  => String
  -> MdbxEnvGeometry
  -> [MdbxEnvFlags]
  -> m MdbxEnv
envOpen path geom flags = do
  (retCreate, env) <- liftIO mdbx_env_create
  checkError () retCreate
  retGeometry <- liftIO $ mdbx_env_set_geometry env sizeMin sizeNow sizeMax growth shrink pageSize
  checkError () retGeometry
  retOpen <- liftIO $ mdbx_env_open env path flags 0o644
  checkError env retOpen
  where
    MdbxEnvGeometry sizeMin sizeNow sizeMax growth shrink pageSize = geom

-- | Close an environment.
envClose
  :: (MonadIO m, MonadFail m)
  => MdbxEnv
  -> m ()
envClose env = do
  ret <- liftIO $ mdbx_env_close env
  checkError () ret

-- | Begins a transaction.
txnBegin
  :: (MonadIO m, MonadFail m)
  => MdbxEnv
  -> Maybe MdbxTxn
  -> [MdbxTxnFlags]
  -> m MdbxTxn
txnBegin env parent flags = do
  (ret, txn) <- liftIO $ mdbx_txn_begin env parent flags
  checkError txn ret

-- | Commits a transaction.
txnCommit
  :: (MonadIO m, MonadFail m)
  => MdbxTxn
  -> m ()
txnCommit txn = do
  ret <- liftIO $ mdbx_txn_commit txn
  checkError () ret

-- | Aborts a transaction.
txnAbort
  :: (MonadIO m, MonadFail m)
  => MdbxTxn
  -> m ()
txnAbort txn = do
  ret <- liftIO $ mdbx_txn_abort txn
  checkError () ret

-- | Opens a database (table).
dbiOpen
  :: (MonadIO m, MonadFail m)
  => MdbxEnv
  -> Maybe String
  -> [MdbxDbFlags]
  -> m MdbxDbi
dbiOpen env name flags = do
  txn <- txnBegin env Nothing []
  (ret, dbi) <- liftIO $ mdbx_dbi_open txn name flags
  txnAbort txn
  checkError dbi ret

-- | Closes a database.
dbiClose
  :: (MonadIO m, MonadFail m)
  => MdbxEnv
  -> MdbxDbi
  -> m ()
dbiClose env dbi = do
  ret <- liftIO $ mdbx_dbi_close env dbi
  checkError () ret

-- | Returns the value associated to the given key, if any.
itemGet
  :: (MonadIO m, MonadFail m)
  => MdbxTxn
  -> MdbxDbi
  -> MdbxVal
  -> m (Maybe MdbxVal)
itemGet txn db key = do
  (ret, val) <- liftIO $ mdbx_get txn db key
  if ret == fromEnum MdbxNotfound
    then return Nothing
    else checkError (Just val) ret

-- | Saves the provided key/value pair.
itemPut
  :: (MonadIO m, MonadFail m)
  => MdbxTxn
  -> MdbxDbi
  -> MdbxVal
  -> MdbxVal
  -> [MdbxPutFlags]
  -> m ()
itemPut txn db key val flags = do
  ret <- liftIO $ mdbx_put txn db key val flags
  checkError () ret

-- | Deletes the value associated with the given key, if any.
itemDel
  :: (MonadIO m, MonadFail m)
  => MdbxTxn
  -> MdbxDbi
  -> MdbxVal
  -> Maybe MdbxVal
  -> m ()
itemDel txn db key mval = do
  ret <- liftIO $ mdbx_del txn db key mval
  checkError () ret

-- | Opens a cursor.
cursorOpen
  :: (MonadIO m, MonadFail m)
  => MdbxTxn
  -> MdbxDbi
  -> m MdbxCursor
cursorOpen txn dbi = do
  (ret, cur) <- liftIO $ mdbx_cursor_open txn dbi
  checkError cur ret

-- | Closes a cursor.
cursorClose
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> m ()
cursorClose cur = do
  liftIO $ mdbx_cursor_close cur

-- | Stores the provided value at the given key, positioning the cursor on it.
cursorPut
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> MdbxVal
  -> MdbxVal
  -> [MdbxPutFlags]
  -> m ()
cursorPut cur key val flags = do
  ret <- liftIO $ mdbx_cursor_put cur key val flags
  checkError () ret

-- | Deletes the value at the current position.
cursorDel
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> [MdbxPutFlags]
  -> m ()
cursorDel cur flags = do
  ret <- liftIO $ mdbx_cursor_del cur flags
  checkError () ret

-- | Moves to the first key on the database.
cursorFirst
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> m (Maybe (MdbxVal, MdbxVal))
cursorFirst cur = cursorMove cur emptyMdbxVal MdbxFirst

-- | Moves to the last key on the database.
cursorLast
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> m (Maybe (MdbxVal, MdbxVal))
cursorLast cur = cursorMove cur emptyMdbxVal MdbxLast

-- | Moves to the given key.
cursorAt
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> MdbxVal
  -> m (Maybe (MdbxVal, MdbxVal))
cursorAt cur key = cursorMove cur key MdbxSetKey

-- | Moves to the given key or first greater than it. Useful for searching.
cursorRange
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> MdbxVal
  -> m (Maybe (MdbxVal, MdbxVal))
cursorRange cur key = cursorMove cur key MdbxSetRange

-- | Moves to the next key.
cursorNext
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> m (Maybe (MdbxVal, MdbxVal))
cursorNext cur = cursorMove cur emptyMdbxVal MdbxNext

-- | Moves to the previous key.
cursorPrev
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> m (Maybe (MdbxVal, MdbxVal))
cursorPrev cur = cursorMove cur emptyMdbxVal MdbxPrev

-- | Moves the cursor using the provided operation.
cursorMove
  :: (MonadIO m, MonadFail m)
  => MdbxCursor
  -> MdbxVal
  -> MdbxCursorOp
  -> m (Maybe (MdbxVal, MdbxVal))
cursorMove cur baseKey op = do
  (ret, key, val) <- liftIO $ mdbx_cursor_get cur baseKey op
  if ret == fromEnum MdbxNotfound
    then return Nothing
    else checkError (Just (key, val)) ret

-- | Converts a Float value to an unsigned Word that can be used by libmdbx's compare function.
keyFromFloat :: Float -> Word32
keyFromFloat = mdbx_key_from_float

-- | Converts a Double value to an unsigned Word that can be used by libmdbx's compare function.
keyFromDouble :: Double -> Word64
keyFromDouble = mdbx_key_from_double

-- | Converts a 32bits signed Int value to an unsigned Word that can be used by libmdbx's compare function.
keyFromInt32 :: Int32 -> Word32
keyFromInt32 = mdbx_key_from_int32

-- | Converts a 64bits signed Int value to an unsigned Word that can be used by libmdbx's compare function.
keyFromInt64 :: Int64 -> Word64
keyFromInt64 = mdbx_key_from_int64

-- | Converts an unsigned Word to a Float value.
floatFromKey :: Word32 -> Float
floatFromKey = mdbx_float_from_key

-- | Converts an unsigned Word to a Double value.
doubleFromKey :: Word64 -> Double
doubleFromKey = mdbx_double_from_key

-- | Converts an unsigned Word value to a 32bits signed Int.
int32FromKey :: Word32 -> Int32
int32FromKey = mdbx_int32_from_key

-- | Converts an unsigned Word value to a 64bits signed Int.
int64FromKey :: Word64 -> Int64
int64FromKey = mdbx_int64_from_key

-- Helpers
checkError
  :: (MonadIO m, MonadFail m)
  => a
  -> Int
  -> m a
checkError val 0 = return val
checkError _ code = do
  msg <- liftIO $ mdbx_strerror code
  fail msg
