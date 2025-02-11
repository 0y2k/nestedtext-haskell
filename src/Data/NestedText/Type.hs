{-# LANGUAGE DeriveGeneric #-}

module Data.NestedText.Type where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Generic.Data (Generic)

type Key = ST.ShortText

data Item
  = Item'String T.Text
  | Item'List (V.Vector Item)
  | Item'Dictionary (M.Map Key Item)
  deriving (Generic, Eq, Show)

data Document
  = Document'Empty
  | Document'Item Item
  deriving (Generic, Eq, Show)
