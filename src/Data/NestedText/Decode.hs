{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.NestedText.Decode where

import Data.NestedText.From
import Data.NestedText.Parse

import Data.Bifunctor (first)
import qualified Data.Text.Lazy as TL
import Generic.Data

data DecodeError a
  = DecodeError'ParseError ParseError
  | DecodeError'FromItemError (FromItemError a)
  deriving (Generic)
instance Eq (FromItemError a) => Eq (DecodeError a) where
  (==) = geq
instance Show (FromItemError a) => Show (DecodeError a) where
  showsPrec = gshowsPrec

decode :: FromItem a => TL.Text -> Either (DecodeError a) a
decode ts = do
  item <- first DecodeError'ParseError $ parse ts
  first DecodeError'FromItemError $ fromItem item
