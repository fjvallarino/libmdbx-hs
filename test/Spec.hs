import Test.Hspec

import qualified Mdbx.ConversionSpec as ConversionSpec
import qualified Mdbx.DatabaseSpec as DatabaseSpec

main :: IO ()
main = do
  -- Run tests
  hspec spec

spec :: Spec
spec = do
  ConversionSpec.spec
  DatabaseSpec.spec
