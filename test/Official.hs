module Official where

import Control.Monad (forM)
import Data.Functor.Identity (Identity(..))
import Data.List (singleton)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import qualified Pipes.Text as PT
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO
  ( IOMode(..), Newline(..)
  , hSetEncoding, hSetNewlineMode
  , nativeNewline, nativeNewlineMode, noNewlineTranslation
  , openFile, utf8
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import qualified Text.JSON as J

import Data.NestedText.Parse
import Data.NestedText.Serialize
import Data.NestedText.Type
import Data.NestedText.Util

data TestLoad
  = TestLoad'Nothing
  | TestLoad'ShouldSuccess FilePath FilePath
  | TestLoad'ShouldFailure FilePath
  deriving (Eq, Show)

data TestDump
  = TestDump'Nothing
  | TestDump'ShouldSuccess FilePath FilePath
  | TestDump'ShouldFailure FilePath
  deriving (Eq, Show)

newtype I = I { unI :: Item }

instance J.JSON I where
  readJSON (J.JSString jsstr) =
    J.Ok $ I $ Item'String $ T.pack $ J.fromJSString jsstr
  readJSON (J.JSArray vs) =
    fmap (I . Item'List . V.fromList) $ forM vs $ fmap unI . J.readJSON
  readJSON (J.JSObject jsobj) =
    fmap (I . Item'Dictionary . M.fromList)
    $ forM (J.fromJSObject jsobj) $ \(k, v) -> do
      i <- J.readJSON v
      return (ST.pack k, unI i)
  readJSON _ = J.Error "cannot convert json to nestedtext item"
  showJSON _ = error "unimplemented"

readPlaintext :: FilePath -> IO TL.Text
readPlaintext file = do
  h <- openFile file ReadMode
  hSetEncoding h utf8
  hSetNewlineMode h noNewlineTranslation
  TLIO.hGetContents h

readJSON :: FilePath -> IO (Maybe Item)
readJSON file = do
  h <- openFile file ReadMode
  hSetEncoding h utf8
  hSetNewlineMode h nativeNewlineMode
  ts <- TLIO.hGetContents h
  -- workaround for json-0.11; not supported trailing whitespaces
  return $ case J.decode $ TL.unpack $ TL.dropWhileEnd (== '\n') ts of
    J.Ok (I i) -> Just $ f i
    J.Error _err -> Nothing
 where
  f = case nativeNewline of
    LF -> id
    CRLF -> bimap (ST.fromText . r . ST.toText) r
  r = T.intercalate osNewline
    . runIdentity . freeTToLines . splitLines . PT.fromLazy . TL.fromStrict
  bimap _fk fv (Item'String ts) = Item'String $ fv ts
  bimap fk fv (Item'List vs) = Item'List $ V.map (bimap fk fv) vs
  bimap fk fv (Item'Dictionary dic) =
    Item'Dictionary $ bimap fk fv <$> M.mapKeys fk dic

test_official :: IO [TestTree]
test_official = do
  let dir = "vendor"
        </> "github.com"
        </> "KenKundert"
        </> "nestedtext_tests"
        </> "test_cases"
      skipLoad =
        -- because invalid value
        [ "empty_1"
        ]
      skipDump =
        -- because disordered keys of dict
        [ "dict_17"
        , "dict_20"
        , "dict_23"
        , "dict_25"
        , "dict_28"
        , "dict_29"
        , "holistic_1"
        , "holistic_2"
        , "holistic_3"
        , "holistic_4"
        , "holistic_5"
        , "holistic_6"
        , "holistic_7"
        -- because trailing colon for inline key
        , "dict_16"
        , "dict_26"
        , "dict_30"
        ]
  ds <- listDirectory dir
  ts <- forM ds $ \d0 -> do
    let d = dir </> d0
        fileLoadIn = d </> "load_in.nt"
        fileLoadOut = d </> "load_out.json"
        fileLoadErr = d </> "load_err.json"
        fileDumpIn = d </> "dump_in.json"
        fileDumpOut = d </> "dump_out.nt"
        fileDumpErr = d </> "dump_err.json"
    fli <- doesFileExist fileLoadIn
    tl <- if not fli then return TestLoad'Nothing else do
      flo <- doesFileExist fileLoadOut
      fle <- doesFileExist fileLoadErr
      return $ case (flo, fle) of
        (True, False) -> TestLoad'ShouldSuccess fileLoadIn fileLoadOut
        (False, True) -> TestLoad'ShouldFailure fileLoadIn
        (True, True) -> error $ d0 ++ ": ambiguous expected result"
        (False, False) -> error $ d0 ++ ": no expected result"
    fdi <- doesFileExist fileDumpIn
    td <- if not fdi then return TestDump'Nothing else do
      fdo <- doesFileExist fileDumpOut
      fde <- doesFileExist fileDumpErr
      return $ case (fdo, fde) of
        (True, False) -> TestDump'ShouldSuccess fileDumpIn fileDumpOut
        (False, True) -> TestDump'ShouldFailure fileDumpIn
        (True, True) -> error $ d0 ++ ": ambiguous expected result"
        (False, False) -> error $ d0 ++ ": no expected result"
    return (d0, tl, td)
  forM ts $ \(d, tl, td) -> do
    tls <- if d `elem` skipLoad then return [] else case tl of
      TestLoad'Nothing -> return []
      TestLoad'ShouldSuccess fin fout -> do
        mjout <- readJSON fout
        case mjout of
          Just iout -> do
            nin <- readPlaintext fin
            return $ singleton $ testCase "load" $ case parse nin of
              Right iin -> iin @?= iout
              Left err -> assertFailure $ show err
          Nothing -> return []
      TestLoad'ShouldFailure fin -> do
        nin <- readPlaintext fin
        return $ singleton $ testCase "load" $ case parse nin of
          Right iin ->
            assertFailure $ "load succeed! it should fail. " ++ show iin
          Left _err -> return ()
    tds <- if d `elem` skipDump then return [] else case td of
      TestDump'Nothing -> return []
      TestDump'ShouldSuccess fin fout -> do
        mjin <- readJSON fin
        case mjin of
          Just iin -> do
            nout <- readPlaintext fout
            return $ singleton $ testCase "dump" $ serialize 4 iin @?= nout
          Nothing -> return []
      TestDump'ShouldFailure fin -> return $ singleton $ testCase "dump" $ do
        mjin <- readJSON fin
        case mjin of
          Just iin -> do
            let _ = iin :: Item
            assertFailure $ "dump succeed! it should fail. " ++ show iin
          Nothing -> return ()
    return $ testGroup d $ tls ++ tds
