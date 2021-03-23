{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (bracket)
import Data.Store
import Data.Text (Text)
import GHC.Generics

import MDBX
import Store

data User = User {
  _username :: Text,
  _password :: Text
} deriving (Eq, Show, Generic, Store)

openEnvDbi = do
  env <- envOpen "./testdb" [MdbxNosubdir, MdbxCoalesce, MdbxLiforeclaim]
  dbi <- dbiOpen env Nothing []
  return (env, dbi)

closeEnvDbi (env, dbi) = do
  envClose env
  print "Done"

main :: IO ()
main = bracket openEnvDbi closeEnvDbi $ \(env, dbi) -> do
  putItems env dbi pairs
  getItems env dbi ["mark", "dale"] >>= print @ [User]
  getItems env dbi ["john", "carl"] >>= print @ [User]
  getItems env dbi (_username <$> users) >>= print @ [User]
  getItemsRange env dbi "dale" "mark" >>= mapM_ (print @ (Text, User))
  where
    users = [
      User "mark" "hide",
      User "john" "test",
      User "ernest" "password",
      User "dale" "done",
      User "carl" "past"
      ]
    pairs = (\u -> (_username u, u)) <$> users
