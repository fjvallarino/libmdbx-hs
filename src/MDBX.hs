{-# LANGUAGE FlexibleContexts #-}

module MDBX (
  MdbxEnv,
  MdbxTxn,
  MdbxDbi,
  MdbxVal(..),
  -- Flags
  MdbxEnvMode(..),
  MdbxEnvFlags(..),
  MdbxTxnFlags(..),
  MdbxDbFlags(..),
  MdbxPutFlags(..),
  MdbxCursorOp(..),
  envOpen,
  envClose,
  txnBegin,
  txnCommit,
  txnAbort,
  dbiOpen,
  dbiClose,
  valPut,
  valGet,
  valDel
) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Internal.MDBX

envOpen :: (MonadIO m, MonadFail m) => String -> [MdbxEnvFlags] -> m MdbxEnv
envOpen path flags = do
  (retCreate, env) <- liftIO mdbx_env_create
  checkError () retCreate
  retOpen <- liftIO $ mdbx_env_open env path flags 0o644
  checkError env retOpen

envClose :: (MonadIO m, MonadFail m) => MdbxEnv -> m ()
envClose env = do
  retClose <- liftIO $ mdbx_env_close env
  checkError () retClose

txnBegin :: (MonadIO m, MonadFail m) => MdbxEnv -> Maybe MdbxTxn -> [MdbxTxnFlags] -> m MdbxTxn
txnBegin env parent flags = do
  (retBegin, txn) <- liftIO $ mdbx_txn_begin env parent flags
  checkError txn retBegin

txnCommit :: (MonadIO m, MonadFail m) => MdbxTxn -> m ()
txnCommit txn = do
  retCommit <- liftIO $ mdbx_txn_commit txn
  checkError () retCommit

txnAbort :: (MonadIO m, MonadFail m) => MdbxTxn -> m ()
txnAbort txn = do
  retAbort <- liftIO $ mdbx_txn_abort txn
  checkError () retAbort

dbiOpen :: (MonadIO m, MonadFail m) => MdbxEnv -> Maybe String -> [MdbxDbFlags] -> m MdbxDbi
dbiOpen env name flags = do
  txn <- txnBegin env Nothing []
  (retOpen, dbi) <- liftIO $ mdbx_dbi_open txn name flags
  txnAbort txn
  checkError dbi retOpen

dbiClose :: (MonadIO m, MonadFail m) => MdbxEnv -> MdbxDbi -> m ()
dbiClose env dbi = do
  retClose <- liftIO $ mdbx_dbi_close env dbi
  checkError () retClose

valGet :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> MdbxVal -> m (Maybe MdbxVal)
valGet txn db key = do
  (retGet, val) <- liftIO $ mdbx_get txn db key
  if retGet == fromEnum MdbxNotfound
    then return Nothing
    else checkError (Just val) retGet

valPut :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> MdbxVal -> MdbxVal -> [MdbxPutFlags] -> m ()
valPut txn db key val flags = do
  retPut <- liftIO $ mdbx_put txn db key val flags
  checkError () retPut

valDel :: (MonadIO m, MonadFail m) => MdbxTxn -> MdbxDbi -> MdbxVal -> Maybe MdbxVal -> m ()
valDel txn db key mval = do
  retDel <- liftIO $ mdbx_del txn db key mval
  checkError () retDel

-- Helpers
checkError :: (MonadIO m, MonadFail m) => a -> Int -> m a
checkError val 0 = return val
checkError _ code = do
  msg <- liftIO $ mdbx_strerror code
  fail msg
