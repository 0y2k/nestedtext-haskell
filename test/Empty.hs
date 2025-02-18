{-# LANGUAGE OverloadedStrings #-}

module Empty where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.NestedText.Parse
import Data.NestedText.Type

test_empty :: TestTree
test_empty = testGroup "empty"
  [ testCase "item" $ do
    parse "" @?= Left ParseError'Empty
  , testCase "document" $ do
    parseDocument "" @?= Right Document'Empty
  ]
