### 0.2.0.0

* Add support for setting database geometry.
* Add `delRange` to remove a range of keys.
* Add `NullByteString` and `NullText` types. The `Store` instances of these
  types are encoded as NULL terminated strings, which allows for using them in
  custom key types with libmdbx. This is not possible with `ByteString` and
  `Text`, since their `Store` instances are serialized with the size field
  first. Given that libmdbx compares keys as an unstructured sequence of bytes,
  this can cause issues since longer strings are considered _greater than_
  shorter ones, even if their content indicates otherwise.
* Since the library now provides `Store` instances for `NullByteString` and
  `NullText`, [Store](https://hackage.haskell.org/package/store) was made a
  dependency for the library itself and not just for the examples.
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
