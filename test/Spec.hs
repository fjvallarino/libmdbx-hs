import Test.Hspec

import qualified Mdbx.DatabaseSpec as DatabaseSpec

main :: IO ()
main = do
  -- Run tests
  hspec spec

spec :: Spec
spec = do
  DatabaseSpec.spec
