{-# LANGUAGE OverloadedStrings #-}

module Util where

import qualified Pipes as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.NestedText.Util

test_lines :: TestTree
test_lines = testGroup "splitLines" $
  let go name textsIn textsOut = testCase name $ do
        linesList <- freeTToLines $ splitLines $ mapM_ P.yield textsIn
        linesList @?= textsOut
   in [ go "LF only in one chunk"
        ["line1\nline2\nline3"]
        ["line1", "line2", "line3"]
      , go "CR only in one chunk"
        ["line1\rline2\rline3"]
        ["line1", "line2", "line3"]
      , go "CRLF only in one chunk"
        ["line1\r\nline2\r\nline3"]
        ["line1", "line2", "line3"]
      , go "CRLF spanning chunk boundary"
        ["line1\r", "\nline2\r", "\nline3"]
        ["line1", "line2", "line3"]
      , go "Mixed newlines and spanning boundaries"
        ["line1", "\r", "\nline2\nli", "ne3\r", "\nline4\rline5"]
        ["line1", "line2", "line3", "line4", "line5"]
      , go "More spanning boundaries"
        ["line1", "\nline2", "\rline3", "\r\nline4\r", "\nline5\r\n", "line6"]
        ["line1", "line2", "line3", "line4", "line5", "line6"]
      , go "Contains empty chunks"
        ["line1", "", "\nline2\n", "", "\nline4"]
        ["line1", "line2", "", "line4"]
      , go "Enclosing blank lines"
        ["\nline2\n"]
        ["", "line2", ""]
      , go "No newline"
        ["single line without newline"]
        ["single line without newline"]
      ]
