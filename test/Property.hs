module Property where

import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.NestedText.Parse
import Data.NestedText.Serialize
import Data.NestedText.Type

genItem :: H.MonadGen m => H.Range Int -> H.Range Int -> H.Range Int -> m Item
genItem rs rk rn = go
 where
  go = HG.recursive HG.choice
    [ Item'String . T.pack <$> HG.string rs char
    ]
    [ Item'List . V.fromList <$> HG.list rn go
    , Item'Dictionary . M.fromList <$> HG.list rn entry
    ]
  entry = do
    k <- ST.pack <$> HG.string rk char
    v <- go
    return (k, v)
  char = HG.filterT C.isPrint HG.unicode

test_property :: TestTree
test_property = testGroup "property"
  [ testProperty "parse . serialize === Right" $ H.property $ do
    n <- H.forAll $ HG.integral $ HR.linear 1 8
    item <- H.forAll $ genItem (HR.linear 0 64) (HR.linear 0 16) (HR.linear 0 32)
    parse (serialize n item) H.=== Right item
  , testProperty "fmap serialize . parse . serialize === Right . serialize" $ H.property $ do
    n <- H.forAll $ HG.integral $ HR.linear 1 8
    item <- H.forAll $ genItem (HR.linear 0 64) (HR.linear 0 16) (HR.linear 0 32)
    (fmap (serialize n) . parse . serialize n) item
      H.=== (Right . serialize n) item
  ]
