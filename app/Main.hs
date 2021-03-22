{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import MDBX
import Store

main :: IO ()
main = do
  env <- envOpen "./testdb" [MdbxNosubdir, MdbxCoalesce, MdbxLiforeclaim]
  dbi <- dbiOpen env Nothing []
  putItems env dbi pairs
--  getItems env ["mark"] >>= \case
--    Right (items :: [User]) -> print items
--    Left ex -> print ex
  getItems env dbi ["mark", "dale"] >>= print @ [User]
  getItems env dbi ["john", "carl"] >>= print @ [User]
  getItems env dbi (_username <$> users) >>= print @ [User]

  envClose env
  print "Done"

  where
    users = [
      User "john" "test",
      User "mark" "hide",
      User "dale" "done",
      User "carl" "past"
      ]
    pairs = (\u -> (_username u, u)) <$> users
