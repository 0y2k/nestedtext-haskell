{-# LANGUAGE DeriveGeneric #-}

module Data.NestedText.Serialize
  ( serialize
  , serialize'
  ) where

import Data.NestedText.Type
import Data.NestedText.Util

import qualified Data.Char as C
import Data.Functor.Identity (Identity(..))
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Generic.Data
import Lens.Micro.Platform
import qualified Pipes.Text as PT

data Denote
  = Denote'Null
  | Denote'SingleLine T.Text
  | Denote'MultipleLine Item
  deriving (Generic)

serialize :: Int -> Item -> TL.Text
serialize indentWidth = TLB.toLazyText . write
 where
  indent i = TLB.fromString $ replicate i ' '
  nl = TLB.fromText osNewline
  isMultilineKey ts =
    isJust (ST.findIndex (== '\n') ts)
    || ST.null ts
    || (case ST.uncons ts of
      Nothing -> False
      Just (c, _) -> C.isSpace c || S.member c specialCharsDict || c == '#')
    || (case ST.unsnoc ts of
      Nothing -> False
      Just (_, c) -> C.isSpace c || c == ':')
    || T.isInfixOf (T.pack ": ") (ST.toText ts)
  denoteItem item@(Item'String ts) = case T.findIndex (== '\n') ts of
    Just _ -> Denote'MultipleLine item
    Nothing -> if T.null ts then Denote'Null else Denote'SingleLine ts
  denoteItem item = Denote'MultipleLine item

  write i = writeItem 0 i <> nl
  writeItem i (Item'String ts) = writeString i ts
  writeItem i (Item'List vs) = if V.null vs
    then indent i <> TLB.fromString "[]"
    else mconcat $ intersperse nl
      $ flip map (V.toList vs) $ \vi -> case denoteItem vi of
        Denote'Null -> indent i <> TLB.fromString "-"
        Denote'SingleLine ts -> indent i <> TLB.fromString "- " <> TLB.fromText ts
        Denote'MultipleLine _ -> indent i <> TLB.fromString "-"
          <> nl <> writeItem (i + indentWidth) vi
  writeItem i (Item'Dictionary dic) = if M.null dic
    then indent i <> TLB.fromString "{}"
    else mconcat $ intersperse nl
      $ flip map (M.toAscList dic) $ \(k, vi) -> if isMultilineKey k
        then writeKey i k <> nl <> writeItem (i + indentWidth) vi
        else case denoteItem vi of
          Denote'Null ->
            indent i <> TLB.fromText (ST.toText k) <> TLB.fromString ":"
          Denote'SingleLine ts ->
            indent i <> TLB.fromText (ST.toText k) <> TLB.fromString ": "
            <> TLB.fromText ts
          Denote'MultipleLine _ ->
            indent i <> TLB.fromText (ST.toText k) <> TLB.fromString ":"
            <> nl <> writeItem (i + indentWidth) vi
  writeString i ts =
    freeTToLines (splitLines $ PT.fromLazy $ TL.fromStrict ts)
    & runIdentity
    & map (\l -> if T.null l
      then indent i <> TLB.fromString ">"
      else indent i <> TLB.fromString "> " <> TLB.fromText l)
    & intersperse nl
    & mconcat
  writeKey i ts =
    freeTToLines (splitLines $ PT.fromLazy $ TL.fromStrict $ ST.toText ts)
    & runIdentity
    & map (\l -> if T.null l
      then indent i <> TLB.fromString ":"
      else indent i <> TLB.fromString ": " <> TLB.fromText l)
    & intersperse nl
    & mconcat

serialize' :: Int -> Item -> T.Text
serialize' i = TL.toStrict . serialize i
