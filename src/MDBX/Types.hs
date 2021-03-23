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
  MdbxItem(..)
) where

import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Foreign.Ptr (castPtr)

import MDBX.FFI

class MdbxItem i where
  fromMdbxVal :: MdbxVal -> IO i
  toMdbxVal :: i -> (MdbxVal -> IO b) -> IO b

instance MdbxItem Text where
  fromMdbxVal (MdbxVal sz ptr) =
    fromPtr (castPtr ptr) (fromIntegral sz `div` 2)

  toMdbxVal key fn = useAsPtr key $ \ptr size ->
    fn $ MdbxVal (fromIntegral size * 2) (castPtr ptr)
