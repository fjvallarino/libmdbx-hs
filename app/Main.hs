{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (bracket)
import Data.Store
import Data.Text (Text)
import GHC.Generics

import MDBX
import MDBX.Store

data User = User {
  _username :: Text,
  _password :: Text
} deriving (Eq, Show, Generic, Store)

deriving via (MdbxItemStore User) instance MdbxItem User

openEnvDbi :: IO MdbxEnv
openEnvDbi = envOpen "./testdb" [MdbxNosubdir, MdbxCoalesce, MdbxLiforeclaim]

main :: IO ()
main = bracket openEnvDbi envClose $ \env -> do
  dbi <- dbiOpen env Nothing []
  putItems env dbi pairs
  getItems @Text env dbi ["mark", "dale"] >>= print @[User]
  getItems @Text env dbi ["john", "carl"] >>= print @[User]
  getItems env dbi (_username <$> users) >>= print @[User]
  getRange @Text env dbi "dale" "mark" >>= mapM_ (print @User)
  where
    users = [
      User "mark" "hide",
      User "john" "test",
      User "ernest" "password",
      User "dale" "done",
      User "carl" "past"
      ]
    pairs = (\u -> (_username u, u)) <$> users
