{-# LANGUAGE OverloadedStrings #-}

module Empty where

import Data.NestedText.Parse
import Data.NestedText.Type

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_empty :: TestTree
test_empty = testGroup "empty"
  [ testCase "item" $ do
    parse "" @?= Left ParseError'Empty
  , testCase "document" $ do
    parseDocument "" @?= Right Document'Empty
  ]
