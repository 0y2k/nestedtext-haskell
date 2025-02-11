{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.NestedText.To where

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor(first))
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

class ToItem a where
  type ToItemError a :: Type
  toItem :: a -> Either (ToItemError a) Item
class ToKey a where
  type ToKeyError a :: Type
  toKey :: a -> Either (ToKeyError a) Key

instance ToItem Item where
  type ToItemError Item = Void
  toItem = Right

data ToItemError'Text
  = ToItemError'Text'Utf8Error
  deriving (Generic, Eq, Show)

instance ToItem BS.ByteString where
  type ToItemError BS.ByteString = ToItemError'Text
  toItem bs = fmap Item'String
    $ first (const ToItemError'Text'Utf8Error) $ TE.decodeUtf8' bs
instance ToItem BL.ByteString where
  type ToItemError BL.ByteString = ToItemError'Text
  toItem bs = fmap Item'String
    $ first (const ToItemError'Text'Utf8Error) $ TE.decodeUtf8' $ BL.toStrict bs
instance ToItem TS.Text where
  type ToItemError TS.Text = Void
  toItem = Right . Item'String
instance ToItem TL.Text where
  type ToItemError TL.Text = Void
  toItem = Right . Item'String . TL.toStrict

data ToItemError'List a
  = ToItemError'List'ElementError (ToItemError a)
  deriving (Generic)
instance Eq (ToItemError a) => Eq (ToItemError'List a) where
  (==) = geq
instance Show (ToItemError a) => Show (ToItemError'List a) where
  showsPrec = gshowsPrec

instance ToItem a => ToItem [a] where
  type ToItemError [a] = ToItemError'List a
  toItem xs = Item'List <$> V.forM (V.fromList xs)
   (first ToItemError'List'ElementError . toItem)
instance ToItem a => ToItem (V.Vector a) where
  type ToItemError (V.Vector a) = ToItemError'List a
  toItem xs = Item'List <$> V.forM xs
   (first ToItemError'List'ElementError . toItem)

data ToKeyError'Text
  = ToKeyError'Text'Utf8Error
  deriving (Generic, Eq, Show)

instance ToKey BS.ByteString where
  type ToKeyError BS.ByteString = ToKeyError'Text
  toKey bs = fmap ST.fromText
    $ first (const ToKeyError'Text'Utf8Error) $ TE.decodeUtf8' bs
instance ToKey BL.ByteString where
  type ToKeyError BL.ByteString = ToKeyError'Text
  toKey bs = fmap ST.fromText
    $ first (const ToKeyError'Text'Utf8Error) $ TE.decodeUtf8' $ BL.toStrict bs
instance ToKey SBS.ShortByteString where
  type ToKeyError SBS.ShortByteString = ToKeyError'Text
  toKey sbs = maybe (Left ToKeyError'Text'Utf8Error) Right
    $ ST.fromShortByteString sbs
instance ToKey TS.Text where
  type ToKeyError TS.Text = Void
  toKey = Right . ST.fromText
instance ToKey TL.Text where
  type ToKeyError TL.Text = Void
  toKey = Right . ST.fromText . TL.toStrict
instance ToKey ST.ShortText where
  type ToKeyError ST.ShortText = Void
  toKey = Right

data ToItemError'Map k v
  = ToItemError'Map'KeyError (ToKeyError k)
  | ToItemError'Map'ValueError (ToItemError v)
  deriving (Generic)
instance (Eq (ToKeyError k), Eq (ToItemError v))
  => Eq (ToItemError'Map k v) where
    (==) = geq
instance (Show (ToKeyError k), Show (ToItemError v))
  => Show (ToItemError'Map k v) where
    showsPrec = gshowsPrec

instance (ToKey k, ToItem v) => ToItem (M.Map k v) where
  type ToItemError (M.Map k v) = ToItemError'Map k v
  toItem dic = fmap (Item'Dictionary . M.fromList)
    $ forM (M.toAscList dic) $ \(ks, vs) -> do
      kd <- first ToItemError'Map'KeyError $ toKey ks
      vd <- first ToItemError'Map'ValueError $ toItem vs
      return (kd, vd)
