module MDBX.Types (
  -- Re-exported from FFI
  MdbxEnv,
  MdbxTxn,
  MdbxDbi,
  MdbxVal(..),
  MdbxEnvMode(..),
  MdbxEnvFlags(..),
  MdbxTxnFlags(..),
  MdbxDbFlags(..),
  MdbxPutFlags(..),
  MdbxCursorOp(..),
  -- Custom types
  MdbxKey(..),
  MdbxItem(..)
) where

import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Foreign.Ptr (castPtr)

import MDBX.FFI

class MdbxKey k where
  fromMdbxKey :: MdbxVal -> IO k
  withMdbxKey :: k -> (MdbxVal -> IO b) -> IO b

class MdbxItem i where
  fromMdbxItem :: MdbxVal -> IO i
  withMdbxItem :: i -> (MdbxVal -> IO b) -> IO b

instance MdbxKey Text where
  fromMdbxKey (MdbxVal sz ptr) =
    fromPtr (castPtr ptr) (fromIntegral sz `div` 2)

  withMdbxKey key fn = useAsPtr key $ \ptr size ->
    fn $ MdbxVal (fromIntegral size * 2) (castPtr ptr)
