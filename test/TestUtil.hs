module TestUtil where

import Control.Exception (SomeException, bracket, catch)
import Data.Default
import System.Directory

import Mdbx

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

removeDB :: IO ()
removeDB = flip catchAny (const $ return ()) $ do
  removeFile "./test.db"
  removeFile "./test.db.lock"

openEnvDbi :: IO MdbxEnv
openEnvDbi = do
  removeDB
  envOpen "./test.db" def [MdbxNosubdir, MdbxCoalesce, MdbxLiforeclaim]

withDatabase :: ((MdbxEnv, MdbxDbi) -> IO ()) -> IO ()
withDatabase runTest = bracket openEnvDbi envClose
  $ \env -> do
    db <- dbiOpen env Nothing []
    runTest (env, db)
