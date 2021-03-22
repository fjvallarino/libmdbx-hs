{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Store (
  User(..),
  getItems,
  putItems
) where

import Control.Monad (forM, forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import Data.Maybe (catMaybes)
import Data.Store (Store, PeekException, decodeIO, encode)
import Data.Traversable (sequenceA)
import Foreign.Ptr
import GHC.Generics

import MDBX

data User = User {
  _username :: String,
  _password :: String
} deriving (Eq, Show, Generic, Store)

getItems :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> [k] -> IO [v]
getItems env dbi keys = do
  txn <- txnBegin env Nothing [MdbxTxnRdonly]

  resp <- forM keys $ \key -> withMVal key $ \mkey -> do
    mval <- valGet txn dbi mkey
    mapM fromMVal mval
  void $ txnCommit txn
  return $ catMaybes resp

putItems :: (Store k, Store v) => MdbxEnv -> MdbxDbi -> [(k, v)] -> IO ()
putItems env dbi items = do
  txn <- txnBegin env Nothing []
  forM_ items $ \(key, val) -> withKeyVal key val $ \mkey mval ->
    valPut txn dbi mkey mval []
  void $ txnCommit txn

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

fromMVal :: Store v => MdbxVal -> IO v
fromMVal (MdbxVal size ptr) = do
  bs <- unsafePackCStringLen (castPtr ptr, fromIntegral size)
  decodeIO bs

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
