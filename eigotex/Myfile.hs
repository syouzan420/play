module Myfile where

import System.IO(IOMode(..), openFile, hClose, hGetContents', hSetEncoding, utf8, hPutStr)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Functor((<&>))

fileRead :: String -> IO String
fileRead fileName = do
  h <- openFile fileName ReadMode
  hSetEncoding h utf8
  hGetContents' h

fileReadT :: FilePath -> IO T.Text
fileReadT fileName = B.readFile fileName <&> decodeUtf8

fileWrite :: String -> String -> IO ()
fileWrite fileName str = do
  h <- openFile fileName WriteMode
  hSetEncoding h utf8
  hPutStr h str
  hClose h
