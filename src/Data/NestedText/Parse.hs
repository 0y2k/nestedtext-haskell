{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.NestedText.Parse
  ( ParseError(..)
  , parse
  , parse'
  , parseDocument
  ) where

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Void
import Generic.Data
import qualified Pipes as P
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import Prelude hiding (readList)

import Data.NestedText.To (ToItem(..), ToKey(..))
import Data.NestedText.Type
import Data.NestedText.Util

data Line
  = Line'Blank
  | Line'Comment Int T.Text
  | Line'StringItem Int T.Text
  | Line'ListItem Int T.Text
  | Line'DictItem Int T.Text (Maybe T.Text)
  | Line'KeyItem Int T.Text
  | Line'InlineList Int T.Text
  | Line'InlineDict Int T.Text
  deriving (Generic, Eq, Ord, Show)

data ValidLine
  = ValidLine'StringItem Int T.Text
  | ValidLine'ListItem Int T.Text
  | ValidLine'DictItem Int T.Text (Maybe T.Text)
  | ValidLine'KeyItem Int T.Text
  | ValidLine'InlineList Int T.Text
  | ValidLine'InlineDict Int T.Text
  deriving (Generic, Eq, Ord, Show)

data ParseError
  = ParseError'BreakingLinesError
  | ParseError'UnrecognizedLine
  | ParseError'InvalidLine
  | ParseError'InvalidIndent
  | ParseError'InvalidChar
  | ParseError'InvalidEndOfLine
  | ParseError'RemainingInlineContent
  | ParseError'DuplicateKey
  | ParseError'RemainingContent
  | ParseError'Empty
  deriving (Generic, Eq, Ord, Show)

toLine :: T.Text -> Either ParseError Line
toLine ts =
  let ts0 = T.unpack ts
      countPrefixSpaces = go 0
       where
        go !n (' ':xs) = go (succ n) xs
        go !n _ = n
      indentLevel = countPrefixSpaces ts0
      spanByFirstColon = go []
       where
        go _ [] = Nothing
        go revks (':':' ':vs) = Just
          ( reverse $ dropWhile isTabOrSpace revks
          , Just vs
          )
        go revks [':'] = Just
          ( reverse $ dropWhile isTabOrSpace revks
          , Nothing
          )
        go revks (x:xs) = go (x:revks) xs
   in case drop indentLevel ts0 of
        [] -> Right Line'Blank
        '#':' ':cs1 -> Right $ Line'Comment indentLevel $ T.pack cs1
        '#':cs1 -> Right $ Line'Comment indentLevel $ T.pack cs1
        ['>'] -> Right $ Line'StringItem indentLevel T.empty
        '>':' ':cs1 -> Right $ Line'StringItem indentLevel $ T.pack cs1
        ['-'] -> Right $ Line'ListItem indentLevel T.empty
        '-':' ':cs1 -> Right $ Line'ListItem indentLevel $ T.pack cs1
        '[':_ -> Right $ Line'InlineList indentLevel ts
        '{':_ -> Right $ Line'InlineDict indentLevel ts
        [':'] -> Right $ Line'KeyItem indentLevel T.empty
        ':':' ':cs1 -> Right $ Line'KeyItem indentLevel $ T.pack cs1
        '\t':_ -> Left ParseError'InvalidIndent
        cs0 -> case spanByFirstColon cs0 of
          Just (k, mv) -> Right $ Line'DictItem indentLevel (T.pack k) (T.pack <$> mv)
          Nothing -> Left ParseError'UnrecognizedLine

toValidLine :: Line -> Maybe ValidLine
toValidLine (Line'StringItem i ts) = Just $ ValidLine'StringItem i ts
toValidLine (Line'ListItem i ts) = Just $ ValidLine'ListItem i ts
toValidLine (Line'DictItem i ts mts) = Just $ ValidLine'DictItem i ts mts
toValidLine (Line'KeyItem i ts) = Just $ ValidLine'KeyItem i ts
toValidLine (Line'InlineList i ts) = Just $ ValidLine'InlineList i ts
toValidLine (Line'InlineDict i ts) = Just $ ValidLine'InlineDict i ts
toValidLine _ = Nothing

validLine'Indent :: ValidLine -> Int
validLine'Indent (ValidLine'StringItem i _) = i
validLine'Indent (ValidLine'ListItem i _) = i
validLine'Indent (ValidLine'DictItem i _ _) = i
validLine'Indent (ValidLine'KeyItem i _) = i
validLine'Indent (ValidLine'InlineList i _) = i
validLine'Indent (ValidLine'InlineDict i _) = i

data WaitingCharList
  = WaitingCharList'ValueOrEnd
  | WaitingCharList'Value
  | WaitingCharList'CommaOrEnd
  deriving (Generic, Eq, Ord, Show)

data WaitingCharDict
  = WaitingCharDict'KeyOrEnd
  | WaitingCharDict'Key
  | WaitingCharDict'Colon Key
  | WaitingCharDict'Value Key
  | WaitingCharDict'CommaOrEnd
  deriving (Generic, Eq, Ord, Show)

parse :: TL.Text -> Either ParseError Item
parse ts0 = StateT.evalStateT parser
  $ PT.folds (<>) T.empty id (splitLines $ PT.fromLazy ts0)
  P.>-> P.mapM toLine
  P.>-> P.mapMaybe toValidLine
 where
  parser = do
    xi <- readItemIndent [EQ] 0
    b <- PP.isEndOfInput
    unless b $ lift $ Left ParseError'RemainingContent
    return xi
  toItemWithoutError :: (ToItem a, ToItemError a ~ Void) => a -> Item
  toItemWithoutError x = case toItem x of
    Right y -> y
    Left _ -> error "toItemWithoutError: unreachable Left"
  toKeyWithoutError :: (ToKey a, ToKeyError a ~ Void) => a -> Key
  toKeyWithoutError x = case toKey x of
    Right y -> y
    Left _ -> error "toKeyWithoutError: unreachable Left"
  removeEnclosingSpace = T.dropWhileEnd C.isSpace . T.dropWhile C.isSpace

  readItemIndent cs i = do
    (item, j) <- readItem
    unless ((i `compare` j) `elem` cs) $ lift $ Left ParseError'InvalidIndent
    return item
  readItem = do
    ml <- PP.peek
    case ml of
      Nothing -> lift $ Left ParseError'Empty
      Just l -> case l of
        ValidLine'StringItem i _ts -> do
          ts <- readString i
          return (toItemWithoutError ts, i)
        ValidLine'ListItem i _ts -> do
          vs <- readList i
          return (Item'List vs, i)
        ValidLine'DictItem i _kts _mvts -> do
          dic <- readDict i
          return (Item'Dictionary dic, i)
        ValidLine'KeyItem i _ts -> do
          dic <- readDict i
          return (Item'Dictionary dic, i)
        ValidLine'InlineList i ts -> do
          _ <- PP.draw
          vs <- parseInlineList ts
          return (Item'List vs, i)
        ValidLine'InlineDict i ts -> do
          _ <- PP.draw
          dic <- parseInlineDict ts
          return (Item'Dictionary dic, i)
  readString i = go []
   where
    go ys = do
      ml <- PP.draw
      case ml of
        Nothing -> return $ T.intercalate osNewline $ reverse ys
        Just x -> case i `compare` validLine'Indent x of
          EQ -> case x of
            ValidLine'StringItem _ ts -> go $ ts : ys
            _ -> lift $ Left ParseError'InvalidLine
          LT -> lift $ Left ParseError'InvalidIndent
          GT -> do
            PP.unDraw x
            return $ T.intercalate osNewline $ reverse ys
  readList i = go []
   where
    go ys = do
      ml <- PP.draw
      case ml of
        Nothing -> return $ V.fromList $ reverse ys
        Just x -> case i `compare` validLine'Indent x of
          EQ -> case x of
            ValidLine'ListItem _ ts -> do
              let yys = toItemWithoutError ts : ys
              ml2 <- PP.peek
              case ml2 of
                Nothing -> go yys
                Just x2 -> case i `compare` validLine'Indent x2 of
                  EQ -> go yys
                  LT -> if not $ T.null ts then go yys else do
                    item <- readItemIndent [LT] i
                    go $ item : ys
                  GT -> return $ V.fromList $ reverse yys
            _ -> lift $ Left ParseError'InvalidLine
          LT -> lift $ Left ParseError'InvalidIndent
          GT -> do
            PP.unDraw x
            return $ V.fromList $ reverse ys
  readDict i = go M.empty
   where
    go m0 = do
      ml <- PP.draw
      case ml of
        Nothing -> return m0
        Just x -> case i `compare` validLine'Indent x of
          EQ -> case x of
            ValidLine'DictItem _ k0 mv ->
              let k = toKeyWithoutError k0 in case mv of
                Just v -> do
                  when (M.member k m0) $ lift $ Left ParseError'DuplicateKey
                  go $ M.insert k (toItemWithoutError v) m0
                Nothing -> do
                  ml2 <- PP.peek
                  case ml2 of
                    Nothing -> do
                      when (M.member k m0) $ lift $ Left ParseError'DuplicateKey
                      go $ M.insert k (toItemWithoutError T.empty) m0
                    Just x2 -> case i `compare` validLine'Indent x2 of
                      EQ -> do
                        when (M.member k m0) $ lift $ Left ParseError'DuplicateKey
                        go $ M.insert k (toItemWithoutError T.empty) m0
                      LT -> do
                        vi <- readItemIndent [LT] i
                        when (M.member k m0) $ lift $ Left ParseError'DuplicateKey
                        go $ M.insert k vi m0
                      GT -> do
                        when (M.member k m0) $ lift $ Left ParseError'DuplicateKey
                        return $ M.insert k (toItemWithoutError T.empty) m0
            ValidLine'KeyItem _ _ts -> do
              PP.unDraw x
              k <- readKey i
              vi <- readItemIndent [LT] i
              when (M.member k m0) $ lift $ Left ParseError'DuplicateKey
              go $ M.insert k vi m0
            _ -> lift $ Left ParseError'InvalidLine
          LT -> lift $ Left ParseError'InvalidIndent
          GT -> do
            PP.unDraw x
            return m0
  readKey i = go []
   where
    go ys = do
      ml <- PP.draw
      case ml of
        Nothing -> return
          $ toKeyWithoutError $ T.intercalate osNewline $ reverse ys
        Just x -> case i `compare` validLine'Indent x of
          EQ -> case x of
            ValidLine'KeyItem _ ts -> go $ ts : ys
            _ -> lift $ Left ParseError'InvalidLine
          _ -> do
            PP.unDraw x
            return $ toKeyWithoutError $ T.intercalate osNewline $ reverse ys

  parseInlineList = lift . StateT.evalStateT go . mapM_ P.yield . T.unpack
   where
    go = do
      vs <- readInlineList0
      readInlineSpace
      b <- PP.isEndOfInput
      unless b $ lift $ Left ParseError'RemainingInlineContent
      return vs
  parseInlineDict = lift . StateT.evalStateT go . mapM_ P.yield . T.unpack
   where
    go = do
      dic <- readInlineDict0
      readInlineSpace
      b <- PP.isEndOfInput
      unless b $ lift $ Left ParseError'RemainingInlineContent
      return dic
  readInlineChar x = do
    mc <- PP.draw
    case mc of
      Nothing -> lift $ Left ParseError'InvalidEndOfLine
      Just c | c == x -> return ()
      Just _ -> lift $ Left ParseError'InvalidChar
  readInlineSpace = do
    mc <- PP.draw
    case mc of
      Nothing -> return ()
      Just c | isTabOrSpace c -> readInlineSpace
      Just c -> do
        PP.unDraw c
        return ()
  readInlineString scs = go []
   where
    go ys = do
      mc <- PP.draw
      case mc of
        Nothing -> return $ removeEnclosingSpace $ T.pack $ reverse ys
        Just c -> if S.notMember c scs then go $ c : ys else do
          PP.unDraw c
          return $ removeEnclosingSpace $ T.pack $ reverse ys
  readInlineList0 = do
    readInlineSpace
    readInlineChar '['
    readInlineList1 WaitingCharList'ValueOrEnd []
  readInlineList1 wcl ys = do
    readInlineSpace
    mc <- PP.draw
    case mc of
      Nothing -> lift $ Left ParseError'InvalidEndOfLine
      Just c -> case wcl of
        WaitingCharList'ValueOrEnd -> case c of
          ']' -> return $ V.fromList $ reverse ys
          '[' -> do
            item <- Item'List <$> readInlineList1 WaitingCharList'ValueOrEnd []
            readInlineList1 WaitingCharList'CommaOrEnd $ item : ys
          '{' -> do
            item <- Item'Dictionary
              <$> readInlineDict1 WaitingCharDict'KeyOrEnd M.empty
            readInlineList1 WaitingCharList'CommaOrEnd $ item : ys
          _ | S.member c specialCharsList ->
            lift $ Left ParseError'InvalidChar
          _ -> do
            PP.unDraw c
            item <- toItemWithoutError <$> readInlineString specialCharsList
            readInlineList1 WaitingCharList'CommaOrEnd $ item : ys
        WaitingCharList'Value -> case c of
          '[' -> do
            item <- Item'List <$> readInlineList1 WaitingCharList'ValueOrEnd []
            readInlineList1 WaitingCharList'CommaOrEnd $ item : ys
          '{' -> do
            item <- Item'Dictionary
              <$> readInlineDict1 WaitingCharDict'KeyOrEnd M.empty
            readInlineList1 WaitingCharList'CommaOrEnd $ item : ys
          _ | S.member c specialCharsList ->
            lift $ Left ParseError'InvalidChar
          _ -> do
            PP.unDraw c
            item <- toItemWithoutError <$> readInlineString specialCharsList
            readInlineList1 WaitingCharList'CommaOrEnd $ item : ys
        WaitingCharList'CommaOrEnd -> case c of
          ',' -> readInlineList1 WaitingCharList'Value ys
          ']' -> return $ V.fromList $ reverse ys
          _ -> lift $ Left ParseError'InvalidChar
  readInlineDict0 = do
    readInlineSpace
    readInlineChar '{'
    readInlineDict1 WaitingCharDict'KeyOrEnd M.empty
  readInlineDict1 wcd m0 = do
    readInlineSpace
    mc <- PP.draw
    case mc of
      Nothing -> lift $ Left ParseError'InvalidEndOfLine
      Just c -> case wcd of
        WaitingCharDict'KeyOrEnd -> case c of
          '}' -> return m0
          _ | S.member c specialCharsDict ->
            lift $ Left ParseError'InvalidChar
          _ -> do
            PP.unDraw c
            key <- toKeyWithoutError <$> readInlineString specialCharsDict
            when (M.member key m0) $ lift $ Left ParseError'DuplicateKey
            readInlineDict1 (WaitingCharDict'Colon key) m0
        WaitingCharDict'Key -> case c of
          _ | S.member c specialCharsDict ->
            lift $ Left ParseError'InvalidChar
          _ -> do
            PP.unDraw c
            key <- toKeyWithoutError <$> readInlineString specialCharsDict
            when (M.member key m0) $ lift $ Left ParseError'DuplicateKey
            readInlineDict1 (WaitingCharDict'Colon key) m0
        WaitingCharDict'Colon k -> case c of
          ':' -> readInlineDict1 (WaitingCharDict'Value k) m0
          _ -> lift $ Left ParseError'InvalidChar
        WaitingCharDict'Value k -> case c of
          '[' -> do
            vi <- Item'List <$> readInlineList1 WaitingCharList'ValueOrEnd []
            readInlineDict1 WaitingCharDict'CommaOrEnd $ M.insert k vi m0
          '{' -> do
            vi <- Item'Dictionary
              <$> readInlineDict1 WaitingCharDict'KeyOrEnd M.empty
            readInlineDict1 WaitingCharDict'CommaOrEnd $ M.insert k vi m0
          _ | S.member c specialCharsDict ->
            lift $ Left ParseError'InvalidChar
          _ -> do
            PP.unDraw c
            vi <- toItemWithoutError <$> readInlineString specialCharsDict
            readInlineDict1 WaitingCharDict'CommaOrEnd $ M.insert k vi m0
        WaitingCharDict'CommaOrEnd -> case c of
          ',' -> readInlineDict1 WaitingCharDict'Key m0
          '}' -> return m0
          _ -> lift $ Left ParseError'InvalidChar

parse' :: T.Text -> Either ParseError Item
parse' = parse . TL.fromStrict

parseDocument :: TL.Text -> Either ParseError Document
parseDocument ts = case parse ts of
  Left ParseError'Empty -> Right Document'Empty
  Right i -> Right $ Document'Item i
  Left err -> Left err
