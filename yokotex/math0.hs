module Main where

import System.Random(randomRIO)
import System.Environment(getArgs)
import qualified Data.Text as T
import Yokotex(makeTex)


makeDoc :: [String] -> IO T.Text
makeDoc arg = undefined

makeHead :: [String] -> T.Text
makeHead arg = undefined

main :: IO ()
main = do
  arg <- getArgs
  makeTex "math0" arg makeDoc makeHead
