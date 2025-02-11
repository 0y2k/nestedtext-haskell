{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.NestedText.From where

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor(first))
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Kind (Type)
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Data.Void (Void)
import Generic.Data

import Data.NestedText.Type

class FromItem a where
  type FromItemError a :: Type
  fromItem :: Item -> Either (FromItemError a) a
class Ord a => FromKey a where
  type FromKeyError a :: Type
  fromKey :: Key -> Either (FromKeyError a) a

instance FromItem Item where
  type FromItemError Item = Void
  fromItem = Right

data FromItemError'Common
  = FromItemError'Common'InvalidValue
  deriving (Generic, Eq, Show)

instance FromItem BS.ByteString where
  type FromItemError BS.ByteString = FromItemError'Common
  fromItem (Item'String t) = Right $ TE.encodeUtf8 t
  fromItem _ = Left FromItemError'Common'InvalidValue
instance FromItem BL.ByteString where
  type FromItemError BL.ByteString = FromItemError'Common
  fromItem (Item'String t) = Right $ BB.toLazyByteString $ TE.encodeUtf8Builder t
  fromItem _ = Left FromItemError'Common'InvalidValue
instance FromItem TS.Text where
  type FromItemError TS.Text = FromItemError'Common
  fromItem (Item'String t) = Right t
  fromItem _ = Left FromItemError'Common'InvalidValue
instance FromItem TL.Text where
  type FromItemError TL.Text = FromItemError'Common
  fromItem (Item'String t) = Right $ TL.fromStrict t
  fromItem _ = Left FromItemError'Common'InvalidValue

data FromItemError'List a
  = FromItemError'List'InvalidValue
  | FromItemError'List'ElementError (FromItemError a)
  deriving (Generic)
instance Eq (FromItemError a) => Eq (FromItemError'List a) where
  (==) = geq
instance Show (FromItemError a) => Show (FromItemError'List a) where
  showsPrec = gshowsPrec

instance FromItem a => FromItem [a] where
  type FromItemError [a] = FromItemError'List a
  fromItem (Item'List xs) = fmap V.toList $ V.forM xs $ \x ->
    first FromItemError'List'ElementError $ fromItem x
  fromItem _ = Left FromItemError'List'InvalidValue
instance FromItem a => FromItem (V.Vector a) where
  type FromItemError (V.Vector a) = FromItemError'List a
  fromItem (Item'List xs) = V.forM xs $ \x ->
    first FromItemError'List'ElementError $ fromItem x
  fromItem _ = Left FromItemError'List'InvalidValue

instance FromKey BS.ByteString where
  type FromKeyError BS.ByteString = Void
  fromKey st = Right $ ST.toByteString st
instance FromKey BL.ByteString where
  type FromKeyError BL.ByteString = Void
  fromKey st = Right $ BB.toLazyByteString $ ST.toBuilder st
instance FromKey SBS.ShortByteString where
  type FromKeyError SBS.ShortByteString = Void
  fromKey st = Right $ ST.toShortByteString st
instance FromKey TS.Text where
  type FromKeyError TS.Text = Void
  fromKey st = Right $ ST.toText st
instance FromKey TL.Text where
  type FromKeyError TL.Text = Void
  fromKey st = Right $ TL.fromStrict $ ST.toText st
instance FromKey ST.ShortText where
  type FromKeyError ST.ShortText = Void
  fromKey = Right

data FromItemError'Map k v
  = FromItemError'Map'InvalidValue
  | FromItemError'Map'KeyError (FromKeyError k)
  | FromItemError'Map'ValueError (FromItemError v)
  deriving (Generic)
instance (Eq (FromKeyError k), Eq (FromItemError v))
  => Eq (FromItemError'Map k v) where
    (==) = geq
instance (Show (FromKeyError k), Show (FromItemError v))
  => Show (FromItemError'Map k v) where
    showsPrec = gshowsPrec

instance (FromKey k, FromItem v) => FromItem (M.Map k v) where
  type FromItemError (M.Map k v) = FromItemError'Map k v
  fromItem (Item'Dictionary dic) =
    fmap M.fromList $ forM (M.toAscList dic) $ \(ks, vs) -> do
      kd <- first FromItemError'Map'KeyError $ fromKey ks
      vd <- first FromItemError'Map'ValueError $ fromItem vs
      return (kd, vd)
  fromItem _ = Left FromItemError'Map'InvalidValue
