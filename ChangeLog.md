### 0.2.0.0

* Add support for setting database geometry.
* Add `delRange` to remove a range of keys.
* Add `NullByteString` type to support having mixed type keys. Because of this,
  the library now depends on [Store](https://hackage.haskell.org/package/store).
* Update to latest version of libmdbx.

### 0.1.0.4

* Require base 4.12 (deriving via is only available since GHC 8.6).
* Remove dependency on string-interpolate.

### 0.1.0.1 - 0.1.0.3

* Fixing Hackage deployment.

### 0.1.0.0

* Initial release.
* Low level API.
* High level API.
