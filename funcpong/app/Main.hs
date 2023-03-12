module Main where

import System.IO (hSetBuffering,hFlush,stdout,BufferMode(NoBuffering))
import Terminal (cls,hideCur,showCur)
import Loop (initiate,inputLoop)

main :: IO ()
main = do
  --hSetBuffering stdout NoBuffering
  cls
  hideCur
  initiate >>= inputLoop
  showCur

