module Data.NestedText.Util where

import qualified Control.Monad.Trans.Free as F
import Data.Functor (($>))
import qualified Data.Set as S
import qualified Data.Text as T
import Lens.Micro.Platform
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import System.IO (Newline(..), nativeNewline)

specialCharsList :: S.Set Char
specialCharsList = S.fromAscList ",[]{}"
specialCharsDict :: S.Set Char
specialCharsDict = S.fromAscList ",:[]{}"

isTabOrSpace :: Char -> Bool
isTabOrSpace c = c == ' ' || c == '\t'

osNewline :: T.Text
osNewline = case nativeNewline of
  LF -> T.pack "\n"
  CRLF -> T.pack "\r\n"

splitLines :: Monad m => P.Producer T.Text m r -> F.FreeT (P.Producer T.Text m) m r
splitLines p0 = F.FreeT $ go1 p0
 where
  go0 r = return $ F.Free $ P.yield T.empty $> F.FreeT (return $ F.Pure r)
  go1 p1 = do
    ec <- P.next p1
    case ec of
      Left r -> go0 r
      Right (txt, p2) -> if T.null txt
        then go1 p2
        else return $ F.Free $ go2 $ P.yield txt >> p2
  go2 p1 = do
    p2 <- p1 ^. PT.break (\c -> c == '\n' || c == '\r')
    return $ F.FreeT $ do
      ec1 <- PT.nextChar p2
      case ec1 of
        Left r -> return $ F.Pure r
        Right ('\n', p3) -> go1 p3
        Right ('\r', p3) -> do
          ec2 <- PT.nextChar p3
          case ec2 of
            Left r -> go0 r
            Right ('\n', p4) -> go1 p4
            Right (c, p4) -> go1 $ P.yield (T.singleton c) >> p4
        Right _ -> error "splitLines: unreachable"

freeTToLines :: Monad m => F.FreeT (P.Producer T.Text m) m r -> m [T.Text]
freeTToLines ft = do
  step <- F.runFreeT ft
  case step of
    F.Pure _ -> return []
    F.Free p0 -> do
      (line, p1) <- P.fold' (<>) T.empty id p0
      rest <- freeTToLines p1
      return $ line : rest
