{-|
Module      : Mdbx.FFI
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Low level bindings to libmdbx functions.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mdbx.FFI (
  -- * Environment
  MdbxEnvFlags(..),
  MdbxEnv(..),
  MdbxEnvMode(..),
  mdbx_env_create,
  mdbx_env_set_geometry,
  mdbx_env_open,
  mdbx_env_close,

  -- * Transaction
  MdbxTxnFlags(..),
  MdbxTxn(..),
  mdbx_txn_begin,
  mdbx_txn_commit,
  mdbx_txn_abort,
  mdbx_txn_env,

  -- * Database
  MdbxDbFlags(..),
  MdbxDbi(..),
  mdbx_dbi_open,
  mdbx_dbi_close,

  -- * Data
  MdbxVal(..),
  emptyMdbxVal,

  -- * CRUD
  MdbxPutFlags(..),
  mdbx_put,
  mdbx_get,
  mdbx_del,

  -- * Cursor
  MdbxCursorOp(..),
  MdbxCursor(..),
  mdbx_cursor_open,
  mdbx_cursor_close,
  mdbx_cursor_del,
  mdbx_cursor_get,
  mdbx_cursor_put,
  mdbx_cursor_count,
  mdbx_strerror,
  mdbx_cmp,

  -- * Error
  MdbxError(..)
) where

import Foreign
import Foreign.C

#include "mdbx.h"

-- Common
{# typedef size_t CSize #}

-- | Error codes for the different operations.
{# enum MDBX_error_t as MdbxError {underscoreToCase} deriving (Show, Eq, Ord) #}

-- Environment

-- | Environment object, needed for all the operations.
{# pointer *MDBX_env as MdbxEnv newtype #}
deriving instance Storable MdbxEnv

-- | Flags for opening an environment.
data MdbxEnvFlags
  = MdbxEnvDefaults
  | MdbxSyncDurable
  | MdbxNosubdir
  | MdbxSafeNosync
  | MdbxMapasync
  | MdbxRdonly
  | MdbxNometasync
  | MdbxWritemap
  | MdbxUtterlyNosync
  | MdbxNotls
  | MdbxExclusive
  | MdbxNordahead
  | MdbxNomeminit
  | MdbxCoalesce
  | MdbxLiforeclaim
  | MdbxPageperturb
  | MdbxAccede
  deriving (Show, Eq, Ord)

--{# enum MDBX_env_flags_t as MdbxEnvFlags {underscoreToCase} deriving (Show, Eq, Ord) #}
{# enum MDBX_env_flags_t as MdbxEnvFlags nocode {underscoreToCase} #}

-- | UNIX permissions to set on created files. Zero value means to open existing, but do not create.
type MdbxEnvMode = {# type mdbx_mode_t  #}
{# typedef mdbx_mode_t MdbxEnvMode #}

-- Opening and closing.

-- | Creates an environment. Represents a database in the file system.
{# fun unsafe mdbx_env_create {alloca- `MdbxEnv' peek*} -> `Int' #}

{--
Haddock ignores these:

- size_lower: Minimum size of the database in bytes.
- size_now: Current size of the database in bytes.
- size_upper: Maximum size of the database in bytes.
- growth_step: Step growth size of the database in bytes. Must be greater than
  zero to allow for growth.
- shrink_threshold: Step shrink size of the database in bytes. Must be greater
  than zero to allow for shrinkage and lower than growth_step to avoid shrinking
  after growth.
- pagesize: Page size of the database in bytes.
--}

{-|
Sets geometry of an environment. All the parameters can receive -1 to keep the
current value. Receives (expressed in bytes): size_lower, size_now, size_upper,
growth_step, shrink_threshold, pagesize.
-}
{# fun unsafe mdbx_env_set_geometry {`MdbxEnv', `Int', `Int', `Int', `Int', `Int', `Int'} -> `Int' #}

-- | Opens an environment. Receives name, flags and mode.
{# fun unsafe mdbx_env_open {`MdbxEnv', `String', bitMask`[MdbxEnvFlags]', `MdbxEnvMode'} -> `Int' #}

-- | Closes an environment.
{# fun unsafe mdbx_env_close {`MdbxEnv'} -> `Int' #}

{-- Closes an env (env, dontSync) --}
-- Fails under ghci
-- {# fun unsafe mdbx_env_close_ex {`MdbxEnv', `Bool'} -> `Int' #}

-- Transaction

-- | Transaction instance. Needed for all operations with data, even reading.
{# pointer *MDBX_txn as MdbxTxn newtype #}
deriving instance Storable MdbxTxn

-- | Flags for a transaction.
data MdbxTxnFlags
  = MdbxTxnReadwrite
  | MdbxTxnNosync
  | MdbxTxnRdonly
  | MdbxTxnNometasync
  | MdbxTxnRdonlyPrepare
  | MdbxTxnTry
  deriving (Show, Eq, Ord)

-- {# enum MDBX_txn_flags_t as MdbxTxnFlags {underscoreToCase} deriving (Show, Eq, Ord) #}
{# enum MDBX_txn_flags_t as MdbxTxnFlags nocode {underscoreToCase} #}

{-|
Begins a new transaction.

Arguments:

- Environment.
- Parent transaction, or Nothing.
- Flags.
-}
{# fun unsafe mdbx_txn_begin {`MdbxEnv', maybeTxn* `Maybe MdbxTxn', bitMask`[MdbxTxnFlags]', alloca- `MdbxTxn' peek*} -> `Int' #}

-- | Commits a transaction.
{# fun unsafe mdbx_txn_commit {`MdbxTxn'} -> `Int' #}

-- | Aborts a transaction.
{# fun unsafe mdbx_txn_abort {`MdbxTxn'} -> `Int' #}

-- | Gets the environment from a transaction.
{# fun unsafe mdbx_txn_env {`MdbxTxn'} -> `MdbxEnv' #}

-- MdbxEnv* mdbx_txn_env	(	const MdbxTxn * 	txn	)

-- Database

-- | Database instance. Represents a logical table in the database.
type MdbxDbi = {# type MDBX_dbi #}
{# typedef MDBX_dbi MdbxDbi #}

-- | Flags for a database.
data MdbxDbFlags
  = MdbxDbDefaults
  | MdbxReversekey
  | MdbxDupsort
  | MdbxIntegerkey
  | MdbxDupfixed
  | MdbxIntegerdup
  | MdbxReversedup
  | MdbxCreate
  | MdbxDbAccede
  deriving (Show, Eq, Ord)

--{# enum MDBX_db_flags_t as MdbxDbFlags {underscoreToCase} deriving (Show, Eq, Ord) #}
{# enum MDBX_db_flags_t as MdbxDbFlags nocode {underscoreToCase} #}

{-|
Opens a database.

Arguments:

- Transaction.
- Name.
- Flags.
-}
{# fun unsafe mdbx_dbi_open {`MdbxTxn', maybeString* `Maybe String', bitMask`[MdbxDbFlags]', alloca- `MdbxDbi' peek*} -> `Int' #}

-- | Closes a database.
{# fun unsafe mdbx_dbi_close {`MdbxEnv', `MdbxDbi'} -> `Int' #}

-- CRUD

-- | Binary blob representing a key or value in the database.
data MdbxVal = MdbxVal {
  mvlSize :: {-# UNPACK #-} !{# type size_t #},
  mvlData :: {-# UNPACK #-} !(Ptr ())
} deriving (Eq, Show)

--instance Show MdbxVal where
--  show (MdbxVal sz dt) = "MdbxVal { size = " ++ show sz ++ ", data = " ++ show dt ++ " }"

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

-- | Sample empty value
emptyMdbxVal :: MdbxVal
emptyMdbxVal = MdbxVal 0 nullPtr

{# pointer *MDBX_val as MdbxValPtr -> MdbxVal #}

-- | Flags for all data related operations.
data MdbxPutFlags
  = MdbxUpsert
  | MdbxNooverwrite
  | MdbxNodupdata
  | MdbxCurrent
  | MdbxAlldups
  | MdbxReserve
  | MdbxAppend
  | MdbxAppenddup
  | MdbxMultiple
  deriving (Show, Eq, Ord)

--{# enum MDBX_put_flags_t as MdbxPutFlags {underscoreToCase} deriving (Show, Eq, Ord) #}
{# enum MDBX_put_flags_t as MdbxPutFlags nocode {underscoreToCase} #}

{-|
Stores a key/value pair.

Arguments:

- Transaction.
- Database.
- Key.
- Value.
-}
{# fun unsafe mdbx_put {`MdbxTxn', `MdbxDbi', with* `MdbxVal', with* `MdbxVal', bitMask`[MdbxPutFlags]'} -> `Int' #}

{-|
Gets a value with the given key.

Arguments:

- Transaction.
- Database.
- Key.
-}
{# fun unsafe mdbx_get {`MdbxTxn', `MdbxDbi', with* `MdbxVal', alloca- `MdbxVal' peek*} -> `Int' #}

{-|
Gets a value with the given key.

Arguments:

- Transaction.
- Database.
- Key.
-}
{# fun unsafe mdbx_del {`MdbxTxn', `MdbxDbi', with* `MdbxVal', withMaybe* `Maybe MdbxVal'} -> `Int' #}

-- Cursor

-- | Cursor instance. Used for efficient navigation in a database.
{# pointer *MDBX_cursor as MdbxCursor newtype #}
deriving instance Storable MdbxCursor

-- | Flags for cursor operations.
data MdbxCursorOp
  = MdbxFirst
  | MdbxFirstDup
  | MdbxGetBoth
  | MdbxGetBothRange
  | MdbxGetCurrent
  | MdbxGetMultiple
  | MdbxLast
  | MdbxLastDup
  | MdbxNext
  | MdbxNextDup
  | MdbxNextMultiple
  | MdbxNextNodup
  | MdbxPrev
  | MdbxPrevDup
  | MdbxPrevNodup
  | MdbxSet
  | MdbxSetKey
  | MdbxSetRange
  | MdbxPrevMultiple
  | MdbxSetLowerbound
  deriving (Show, Eq, Ord)

--{# enum MDBX_cursor_op as MdbxCursorOp {underscoreToCase} deriving (Show, Eq, Ord) #}
{# enum MDBX_cursor_op as MdbxCursorOp nocode {underscoreToCase} #}

{-|
Opens a new cursor.

Arguments:

- Transaction.
- Database.
-}
{# fun unsafe mdbx_cursor_open {`MdbxTxn', `MdbxDbi', alloca- `MdbxCursor' peek*} -> `Int' #}

-- | Closes a cursor.
{# fun unsafe mdbx_cursor_close {`MdbxCursor'} -> `()' #}

-- | Removes the current key/value pair.
{# fun unsafe mdbx_cursor_del {`MdbxCursor', bitMask`[MdbxPutFlags]'} -> `Int' #}

-- | Returns the current key/value pair.
{# fun unsafe mdbx_cursor_get {`MdbxCursor', with* `MdbxVal' peek*, alloca- `MdbxVal' peek*, `MdbxCursorOp'} -> `Int' #}

{-| Sets the value for the current key.

Arguments:

- Cursor.
- Key.
- Value.
- FLags.
-}
{# fun unsafe mdbx_cursor_put {`MdbxCursor', with* `MdbxVal', with* `MdbxVal', bitMask`[MdbxPutFlags]'} -> `Int' #}

-- | Returns the count of duplicates in the current key.
{# fun unsafe mdbx_cursor_count {`MdbxCursor', alloca- `CSize' peek*} -> `Int' #}

-- Helpers

-- | Returns the description of a given error number.
{# fun unsafe mdbx_strerror {`Int'} -> `String' #}

-- | Compares two values as a binary blob.
{# fun unsafe mdbx_cmp {`MdbxTxn', `MdbxDbi', with* `MdbxVal', with* `MdbxVal'} -> `Int' #}

bitMask :: Enum a => [a] -> C2HSImp.CInt
bitMask = foldl (.|.) 0 . fmap (fromIntegral . fromEnum)

maybeString :: Maybe String -> (Ptr CChar -> IO c) -> IO c
maybeString Nothing fn = fn nullPtr
maybeString (Just val) fn = withCString val fn

maybeTxn :: Maybe MdbxTxn -> (MdbxTxn -> IO c) -> IO c
maybeTxn Nothing fn = fn (MdbxTxn nullPtr)
maybeTxn (Just val) fn = fn val

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO c) -> IO c
withMaybe Nothing fn = fn nullPtr
withMaybe (Just val) fn = with val fn
