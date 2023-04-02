module Myfile where

import System.IO(IOMode(..), openFile, hClose, hGetContents', hSetEncoding, utf8, hPutStr)

fileRead :: String -> IO String
fileRead fileName = do
  h <- openFile fileName ReadMode
  hSetEncoding h utf8
  hGetContents' h

fileWrite :: String -> String -> IO ()
fileWrite fileName str = do
  h <- openFile fileName WriteMode
  hSetEncoding h utf8
  hPutStr h str
  hClose h
