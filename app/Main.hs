{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (forM_)
import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Internal.LibMDX
import Lib

import Store

main :: IO ()
main = do
  putStrLn "First time"
  runDB
--  putStrLn "Second time"
--  runDB
--  putStrLn "Exiting"

runDB :: IO ()
runDB = do
  (_, env) <- mdbx_env_create
  _ <- mdbx_env_open env "./testdb" [MdbxNosubdir, MdbxCoalesce, MdbxLiforeclaim] 0o644
  putItems env pairs
--  getItems env ["mark"] >>= \case
--    Right (items :: [User]) -> print items
--    Left ex -> print ex
  getItems env ["mark", "dale"] >>= \case
    Right (items :: [User]) -> print items
    Left ex -> print ex

  mdbx_env_close env
  print "Done"

  where
    users = [
      User "john" "test",
      User "mark" "hide",
      User "dale" "done",
      User "carl" "past"
      ]
    pairs = (\u -> (_username u, u)) <$> users

{--
runDB :: IO ()
runDB = do
  (r1, env) <- mdbx_env_create
  print ("Create", r1)

  r2 <- mdbx_env_open env "./testdb" [MdbxNosubdir, MdbxCoalesce, MdbxLiforeclaim] 0o644
  r3 <- mdbx_strerror r2
  print ("Open", r2, r3)

  (r4, txn) <- mdbx_txn_begin env Nothing []
  print ("Begin", r4)

  (r5, dbi) <- mdbx_dbi_open txn Nothing []
  print ("Open", r5)

  (key1, val1, pair1) <- newPair (10 :: Int) (123 :: Int)
  (key2, val2, pair2) <- newPair (11 :: Int) (737 :: Int)
  (key3, val3, pair3) <- newPair (12 :: Int) (987 :: Int)

  r6a <- mdbx_put txn dbi key1 val1 []
  r6b <- mdbx_put txn dbi key2 val2 []
  r6c <- mdbx_put txn dbi key3 val3 []
  print ("Put", r6a, r6b, r6c)

  (r7, val1aP) <- mdbx_get txn dbi key1
  vala2 :: Int <- peek (castPtr $ mvlData val1aP)
  print ("Get", r7, vala2)

  r8 <- mdbx_txn_commit txn
  print ("Commit", r8)

  rt2 <- mdbx_put txn dbi key1 val1 []

  (r4, txn) <- mdbx_txn_begin env Nothing []
  (r9, cur1) <- mdbx_cursor_open txn dbi

  (r10, keyC1, valC1) <- mdbx_cursor_get cur1 MdbxNext
  printCursorStep"Cursor get 1" r10 keyC1 valC1
  (r11, keyCount1) <- mdbx_cursor_count cur1
  print ("Key count", r11, keyCount1)
  (r12, keyC2, valC2) <- mdbx_cursor_get cur1 MdbxNext
  printCursorStep"Cursor get 2" r12 keyC2 valC2
  (r13, keyC3, valC3) <- mdbx_cursor_get cur1 MdbxNext
  printCursorStep"Cursor get 3" r13 keyC3 valC3
  (r14, keyC4, valC4) <- mdbx_cursor_get cur1 MdbxNext
  printCursorStep"Cursor get 4" r14 keyC4 valC4

  mdbx_cursor_close cur1
  r15 <- mdbx_txn_commit txn

  freePair pair1
  freePair pair2
  freePair pair3

  resClose <- mdbx_env_close env
  print ("Close", resClose)

printCursorStep desc res key val = do
  if res == 0
    then do
      ks <- showIntVal key
      vs <- showIntVal val
      print (desc, res, ks, vs)
    else
      print ("Failed", desc, res)

showIntVal :: MdbxVal -> IO Int
showIntVal = showVal

showVal :: Storable a => MdbxVal -> IO a
showVal val = peek (castPtr $ mvlData val)

newPair :: (Storable k, Storable v) => k -> v -> IO (MdbxVal, MdbxVal, (Ptr (), Ptr ()))
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
