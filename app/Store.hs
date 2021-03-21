{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Store (
  User(..),
  getItems,
  putItems
) where

import Control.Monad (forM, forM_, void)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Store (Store, PeekException, decode, encode)
import Data.Traversable (sequenceA)
import Foreign.Ptr
import GHC.Generics

import Internal.LibMDX

data User = User {
  _username :: String,
  _password :: String
} deriving (Eq, Show, Generic, Store)

printError :: Show a => a -> Int -> IO ()
printError msg err = do
  desc <- mdbx_strerror err
  print (msg, err, desc)

getItems :: (Store k, Store v) => MdbxEnv -> [k] -> IO (Either PeekException [v])
getItems env keys = do
  (r1, txn) <- mdbx_txn_begin env Nothing [MdbxTxnRdonly]
  printError "r1" r1
  (r2, dbi) <- mdbx_dbi_open txn Nothing []
  printError "r2" r2
  resp <- forM keys $ \key -> withMVal key $ \mkey -> do
    (r3, mval) <- mdbx_get txn dbi mkey
    printError "r3" r3
    fromMVal mval
  void $ mdbx_txn_commit txn
  return $ sequenceA resp

putItems :: (Store k, Store v) => MdbxEnv -> [(k, v)] -> IO ()
putItems env items = do
  (_, txn) <- mdbx_txn_begin env Nothing []
  (_, dbi) <- mdbx_dbi_open txn Nothing []
  forM_ items $ \(key, val) -> withKeyVal key val $ \mkey mval ->
    mdbx_put txn dbi mkey mval []
  void $ mdbx_txn_commit txn

withMVal :: Store v => v -> (MdbxVal -> IO a) -> IO a
withMVal val fn = unsafeUseAsCStringLen bs $ \(ptr, size) ->
  fn $ MdbxVal (fromIntegral size) (castPtr ptr)
  where
    bs = encode val

withKeyVal :: (Store k, Store v) => k -> v -> (MdbxVal -> MdbxVal -> IO a) -> IO a
withKeyVal key val fn =
  unsafeUseAsCStringLen bsK $ \(ptrK, sizeK) ->
    unsafeUseAsCStringLen bsV $ \(ptrV, sizeV) -> do
      let mkey = MdbxVal (fromIntegral sizeK) (castPtr ptrK)
      let mval = MdbxVal (fromIntegral sizeV) (castPtr ptrV)
      fn mkey mval
  where
    bsK = encode key
    bsV = encode val

fromMVal :: Store v => MdbxVal -> IO (Either PeekException v)
fromMVal (MdbxVal size ptr) = do
  bs <- unsafePackCStringLen (castPtr ptr, fromIntegral size)
  return $ decode bs

{--
newPair :: (Store k, Store v) => k -> v -> IO (MdbxVal, MdbxVal, (Ptr (), Ptr ()))
newPair key val = do
  (mkey, pkey) <- newMdbxVal key
  (mval, pval) <- newMdbxVal val
  return (mkey, mval, (pkey, pval))

freePair :: (Ptr (), Ptr ()) -> IO ()
freePair (key, val) = do
  free key
  free val

newMdbxVal :: Storable a => a -> IO (MdbxVal, Ptr ())
newMdbxVal val = do
  valP <- malloc
  poke valP val
  return (mk valP, castPtr valP)
  where
    mk ptr = MdbxVal (fromIntegral $ sizeOf val) (castPtr ptr)
--}
