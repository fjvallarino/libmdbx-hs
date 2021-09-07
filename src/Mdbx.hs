{-|
Module      : Mdbx
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Bindings for libmdbx, a high-performance, in-process, key-value store.

This is the module most applications should import.

See the "Mdbx.Types" module for details on how to make your data types work with
libmdbx. It is recommended to use a serialization library such as
<https://hackage.haskell.org/package/binary binary> to simplify this task.

For the high level API, see the "Mdbx.Database" module.

For the low level API, or if you want to use cursors, see "Mdbx.API".

"Mdbx.FFI" (not exported by this module) provides direct, low level, bindings to
libmdbx.
-}
module Mdbx (
  module Mdbx.API,
  module Mdbx.Binary,
  module Mdbx.Database,
  module Mdbx.Store,
  module Mdbx.Types
) where

import Mdbx.API
import Mdbx.Binary
import Mdbx.Database
import Mdbx.Store
import Mdbx.Types
