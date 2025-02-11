{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.NestedText.Encode where

import Data.Bifunctor (first)
import qualified Data.Text.Lazy as TL
import Generic.Data

import Data.NestedText.To
import Data.NestedText.Serialize

data EncodeError a
  = EncodeError'ToItemError (ToItemError a)
  deriving (Generic)
instance Eq (ToItemError a) => Eq (EncodeError a) where
  (==) = geq
instance Show (ToItemError a) => Show (EncodeError a) where
  showsPrec = gshowsPrec

encode :: ToItem a => a -> Either (EncodeError a) TL.Text
encode = fmap (serialize 2) . first EncodeError'ToItemError . toItem
