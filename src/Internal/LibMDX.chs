{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Internal.LibMDX where

import Data.Bits
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

#include "mdbx.h"

-- | Combine the values in the set using a bitwise or
bitMask :: Enum a => [a] -> C2HSImp.CInt
bitMask = foldl (.|.) 0 . fmap (fromIntegral . fromEnum)

{# typedef size_t CSize #}

-- Environment
{# pointer *MDBX_env as MdbxEnv newtype #}
deriving instance Storable MdbxEnv

{# enum MDBX_env_flags_t as MdbxEnvFlags {underscoreToCase} deriving (Show, Eq, Ord) #}

type MdbxEnvMode = {# type mdbx_mode_t  #}
{# typedef mdbx_mode_t MdbxEnvMode #}

-- Helpers

{# fun unsafe mdbx_strerror {`Int'} -> `String' #}

-- Opening and closing
{-- Creates an env --}
{# fun unsafe mdbx_env_create {alloca- `MdbxEnv' peek*} -> `Int' #}

{-- Open an env (env, name, flags, mode) --}
{# fun unsafe mdbx_env_open {`MdbxEnv', `String', bitMask`[MdbxEnvFlags]', `MdbxEnvMode'} -> `Int' #}

{-- Closes an env --}
{# fun unsafe mdbx_env_close {`MdbxEnv'} -> `Int' #}

{-- Closes an env (env, dontSync) --}
-- Fails under ghci
-- {# fun unsafe mdbx_env_close_ex {`MdbxEnv', `Bool'} -> `Int' #}

-- Transaction
{# pointer *MDBX_txn as MdbxTxn newtype #}
deriving instance Storable MdbxTxn

{# enum MDBX_txn_flags_t as MdbxTxnFlags {underscoreToCase} deriving (Show, Eq, Ord) #}

{-- Creates a new transaction (env, parent txn, flags) --}
{# fun unsafe mdbx_txn_begin {`MdbxEnv', maybeTxn* `Maybe MdbxTxn', bitMask`[MdbxTxnFlags]', alloca- `MdbxTxn' peek*} -> `Int' #}

{-- Commits a transaction --}
{# fun unsafe mdbx_txn_commit {`MdbxTxn'} -> `Int' #}

{-- Aborts a transaction --}
{# fun unsafe mdbx_txn_abort {`MdbxTxn'} -> `Int' #}

-- Database
type MdbxDbi = {# type MDBX_dbi #}
{# typedef MDBX_dbi MdbxDbi #}

{# enum MDBX_db_flags_t as MdbxDbFlags {underscoreToCase} deriving (Show, Eq, Ord) #}

{-- Opens a database (txn, name, flags) --}
{# fun unsafe mdbx_dbi_open {`MdbxTxn', maybeString* `Maybe String', bitMask`[MdbxDbFlags]', alloca- `MdbxDbi' peek*} -> `Int' #}

{-- Closes a database (env, dbi) --}
{# fun unsafe mdbx_dbi_close {`MdbxEnv', `MdbxDbi'} -> `Int' #}

-- CRUD
data MdbxVal = MdbxVal {
  mvlSize :: {-# UNPACK #-} !{# type size_t #},
  mvlData :: {-# UNPACK #-} !(Ptr ())
}

instance Storable MdbxVal where
  sizeOf _ = {# sizeof MDBX_val #}
  alignment _ = {# alignof MDBX_val #}
  peek ptr = do
    sz <- {# get MDBX_val->iov_len #} ptr
    pd <- {# get MDBX_val->iov_base #} ptr
    return $! MdbxVal sz pd
  poke ptr (MdbxVal sz pd) = do
    {# set MDBX_val->iov_len #} ptr sz
    {# set MDBX_val->iov_base #} ptr pd

{# pointer *MDBX_val as MdbxValPtr -> MdbxVal #}

{# enum MDBX_put_flags_t as MdbxPutFlags {underscoreToCase} deriving (Show, Eq, Ord) #}

{-- Stores a value (txn, dbi, key, value) --}
{# fun unsafe mdbx_put {`MdbxTxn', `MdbxDbi', with* `MdbxVal', with* `MdbxVal', bitMask`[MdbxPutFlags]'} -> `Int' #}

{-- Gets a value with the given key (txn, dbi, key) --}
{# fun unsafe mdbx_get {`MdbxTxn', `MdbxDbi', with* `MdbxVal', alloca- `MdbxVal' peek*} -> `Int' #}

{-- Gets a value with the given key (txn, dbi, key) --}
{# fun unsafe mdbx_del {`MdbxTxn', `MdbxDbi', with* `MdbxVal', withMaybe* `Maybe MdbxVal'} -> `Int' #}

-- Cursor
{# pointer *MDBX_cursor as MdbxCursor newtype #}
deriving instance Storable MdbxCursor

{# enum MDBX_cursor_op as MdbxCursorOp {underscoreToCase} deriving (Show, Eq, Ord) #}

{-- Opens a new curspr (txn, dbi) --}
{# fun unsafe mdbx_cursor_open {`MdbxTxn', `MdbxDbi', alloca- `MdbxCursor' peek*} -> `Int' #}

{-- Closes a cursor (txn, dbi) --}
{# fun unsafe mdbx_cursor_close {`MdbxCursor'} -> `()' #}

{-- Returns the count of duplicates in the current key (txn, dbi) --}
{# fun unsafe mdbx_cursor_count {`MdbxCursor', alloca- `CSize' peek*} -> `Int' #}

{-- Removes the current key/value pair (cursor, flags) --}
{# fun unsafe mdbx_cursor_del {`MdbxCursor', bitMask`[MdbxPutFlags]'} -> `Int' #}

{-- Returns the current key/value pair (cursor, cursorOp) --}
{# fun unsafe mdbx_cursor_get {`MdbxCursor', alloca- `MdbxVal' peek*, alloca- `MdbxVal' peek*, `MdbxCursorOp'} -> `Int' #}

{-- Sets the value for the current key (cursor, key, value, flags) --}
{# fun unsafe mdbx_cursor_put {`MdbxCursor', with* `MdbxVal', with* `MdbxVal', bitMask`[MdbxPutFlags]'} -> `Int' #}

maybeString :: Maybe String -> (Ptr CChar -> IO c) -> IO c
maybeString Nothing fn = fn nullPtr
maybeString (Just val) fn = withCString val fn

maybeTxn :: Maybe MdbxTxn -> (MdbxTxn -> IO c) -> IO c
maybeTxn Nothing fn = fn (MdbxTxn nullPtr)
maybeTxn (Just val) fn = fn val

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO c) -> IO c
withMaybe Nothing fn = fn nullPtr
withMaybe (Just val) fn = with val fn
