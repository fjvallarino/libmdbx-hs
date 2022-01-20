### 0.2.1.1

* Upgrade libmdbx to latest version (30b3cc3).
* Properly close cursor after use.
* Document the need of including `MdbxNotls` when using the multi-threaded runtime.

### 0.2.1.0

* Add proper transaction handling for Database module (commit on success but
  also abort on exception).
* Add `getBounds` function to retrieve minimum and maximum key/pairs on a given
  range of keys.

### 0.2.0.0

* Add support for setting database geometry.
* Add `delRange` to remove a range of keys.
* Add `getRangePairs` to retrieve the keys alongside the values.
* Add `NullByteString` and `NullText` types. The `Binary` and `Store` instances
  for these types are encoded as NULL terminated strings, which allows for using
  them in custom key types with libmdbx. This is not possible with `ByteString`
  and `Text`, since their `Binary` and `Store` instances are serialized with the
  size field first. Given that libmdbx compares keys as an unstructured sequence
  of bytes, this can cause issues since longer strings are considered _greater
  than_ shorter ones, even if their content indicates otherwise.
* Since the library now provides `Store` instances for `NullByteString` and
  `NullText`, [Store](https://hackage.haskell.org/package/store) was made a
  dependency for the library itself and not just for the examples.
* The same happens with [Binary](https://hackage.haskell.org/package/binary). It
  is also marked as the recommended serialization library for keys since it
  uses big endian for encoding, eliminating issues when comparing keys with
  libmdbx's comparison function.
* Make `Database` module Strict. Recommend making user types strict.
* Update to latest version of libmdbx.
* Add test cases.

### 0.1.0.4

* Require base 4.12 (deriving via is only available since GHC 8.6).
* Remove dependency on string-interpolate.

### 0.1.0.1 - 0.1.0.3

* Fixing Hackage deployment.

### 0.1.0.0

* Initial release.
* Low level API.
* High level API.
