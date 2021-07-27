{-|
Module      : Mdbx
Copyright   : (c) 2021 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for libmdbx-hs. This is what most applications should import.

In case you only need to store, update and retrieve data, check the
`Mdbx.Database` module.

You will also want to check `Mdbx.Types` in order to be able to store your data
types. In general, you will also want to use a serialization library such as
[store](https://hackage.haskell.org/package/store) or
[cereal](https://hackage.haskell.org/package/cereal).

If you want fine grained control or using cursors, check `Mdbx.API`.

The `Mdbx.FFI` (not exported by this module) provides direct, low level,
bindings to libmdbx.
-}
module Mdbx (
  module Mdbx.API,
  module Mdbx.Database,
  module Mdbx.Types
) where

import Mdbx.API
import Mdbx.Database
import Mdbx.Types
