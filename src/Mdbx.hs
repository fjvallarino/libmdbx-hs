{-|
Module      : Mdbx
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Bindings for libmdbx, a high-performance, in-process, key-value store.

This is the module most applications should import.

For the high level API, check the "Mdbx.Database" module.

For the low level API, or if you need finer grained control/using cursors, check
"Mdbx.API".

You will also want to check "Mdbx.Types" to be able to store your data types. In
general, you will also want to use a serialization library such as
<https://hackage.haskell.org/package/store store>.

"Mdbx.FFI" (not exported by this module) provides direct, low level, bindings to
libmdbx.
-}
module Mdbx (
  module Mdbx.API,
  module Mdbx.Database,
  module Mdbx.Store,
  module Mdbx.Types
) where

import Mdbx.API
import Mdbx.Database
import Mdbx.Store
import Mdbx.Types
