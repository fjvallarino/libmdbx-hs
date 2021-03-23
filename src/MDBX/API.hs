{-# LANGUAGE FlexibleContexts #-}

module MDBX.API (
  -- Keys
  keyCmp,
  -- Env
  envOpen,
  envClose,
  -- Txn
  txnBegin,
  txnCommit,
  txnAbort,
  -- Dbi
  dbiOpen,
  dbiClose,
  -- Val
  itemPut,
  itemGet,
  itemDel,
  -- Cursor
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
  cursorMove
) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import MDBX.FFI

keyCmp :: MonadIO m => MdbxTxn -> MdbxDbi -> MdbxVal -> MdbxVal -> m Int
keyCmp txn dbi key1 key2 = liftIO $ mdbx_cmp txn dbi key1 key2

envOpen :: (MonadIO m, MonadFail m) => String -> [MdbxEnvFlags] -> m MdbxEnv
envOpen path flags = do
  (retCreate, env) <- liftIO mdbx_env_create
  checkError () retCreate
  retOpen <- liftIO $ mdbx_env_open env path flags 0o644
  checkError env retOpen

envClose :: (MonadIO m, MonadFail m) => MdbxEnv -> m ()
envClose env = do
  ret <- liftIO $ mdbx_env_close env
  checkError () ret

txnBegin :: (MonadIO m, MonadFail m) => MdbxEnv -> Maybe MdbxTxn -> [MdbxTxnFlags] -> m MdbxTxn
txnBegin env parent flags = do
  (ret, txn) <- liftIO $ mdbx_txn_begin env parent flags
  checkError txn ret

txnCommit :: (MonadIO m, MonadFail m) => MdbxTxn -> m ()
txnCommit txn = do
  ret <- liftIO $ mdbx_txn_commit txn
  checkError () ret

txnAbort :: (MonadIO m, MonadFail m) => MdbxTxn -> m ()
txnAbort txn = do
  ret <- liftIO $ mdbx_txn_abort txn
  checkError () ret

dbiOpen :: (MonadIO m, MonadFail m) => MdbxEnv -> Maybe String -> [MdbxDbFlags] -> m MdbxDbi
dbiOpen env name flags = do
  txn <- txnBegin env Nothing []
  (ret, dbi) <- liftIO $ mdbx_dbi_open txn name flags
  txnAbort txn
  checkError dbi ret

dbiClose :: (MonadIO m, MonadFail m) => MdbxEnv -> MdbxDbi -> m ()
dbiClose env dbi = do
  ret <- liftIO $ mdbx_dbi_close env dbi
  checkError () ret

itemGet :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> MdbxVal -> m (Maybe MdbxVal)
itemGet txn db key = do
  (ret, val) <- liftIO $ mdbx_get txn db key
  if ret == fromEnum MdbxNotfound
    then return Nothing
    else checkError (Just val) ret

itemPut :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> MdbxVal -> MdbxVal -> [MdbxPutFlags] -> m ()
itemPut txn db key val flags = do
  ret <- liftIO $ mdbx_put txn db key val flags
  checkError () ret

itemDel :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> MdbxVal -> Maybe MdbxVal -> m ()
itemDel txn db key mval = do
  ret <- liftIO $ mdbx_del txn db key mval
  checkError () ret

cursorOpen :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> m MdbxCursor
cursorOpen txn dbi = do
  (ret, cur) <- liftIO $ mdbx_cursor_open txn dbi
  checkError cur ret

cursorClose :: (MonadIO m, MonadFail m) => MdbxCursor -> m ()
cursorClose cur = do
  liftIO $ mdbx_cursor_close cur

cursorPut :: (MonadIO m, MonadFail m) => MdbxCursor -> MdbxVal -> MdbxVal -> [MdbxPutFlags] -> m ()
cursorPut cur key val flags = do
  ret <- liftIO $ mdbx_cursor_put cur key val flags
  checkError () ret

cursorDel :: (MonadIO m, MonadFail m) => MdbxCursor -> [MdbxPutFlags] -> m ()
cursorDel cur flags = do
  ret <- liftIO $ mdbx_cursor_del cur flags
  checkError () ret

cursorFirst :: (MonadIO m, MonadFail m) => MdbxCursor -> m (Maybe (MdbxVal, MdbxVal))
cursorFirst cur = cursorMove cur  emptyMdbxVal MdbxFirst

cursorLast :: (MonadIO m, MonadFail m) => MdbxCursor -> m (Maybe (MdbxVal, MdbxVal))
cursorLast cur = cursorMove cur emptyMdbxVal MdbxLast

cursorAt :: (MonadIO m, MonadFail m) => MdbxCursor -> MdbxVal -> m (Maybe (MdbxVal, MdbxVal))
cursorAt cur key = cursorMove cur key MdbxSetKey

cursorRange :: (MonadIO m, MonadFail m) => MdbxCursor -> MdbxVal -> m (Maybe (MdbxVal, MdbxVal))
cursorRange cur key = cursorMove cur key MdbxSetRange

cursorNext :: (MonadIO m, MonadFail m) => MdbxCursor -> m (Maybe (MdbxVal, MdbxVal))
cursorNext cur = cursorMove cur emptyMdbxVal MdbxNext

cursorPrev :: (MonadIO m, MonadFail m) => MdbxCursor -> m (Maybe (MdbxVal, MdbxVal))
cursorPrev cur = cursorMove cur emptyMdbxVal MdbxPrev

cursorMove :: (MonadIO m, MonadFail m) => MdbxCursor -> MdbxVal -> MdbxCursorOp -> m (Maybe (MdbxVal, MdbxVal))
cursorMove cur baseKey op = do
  (ret, key, val) <- liftIO $ mdbx_cursor_get cur baseKey op
  if ret == fromEnum MdbxNotfound
    then return Nothing
    else checkError (Just (key, val)) ret

-- Helpers
checkError :: (MonadIO m, MonadFail m) => a -> Int -> m a
checkError val 0 = return val
checkError _ code = do
  msg <- liftIO $ mdbx_strerror code
  fail msg
