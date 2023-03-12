module Terminal where

import System.IO.HiddenChar

cls :: IO ()
cls = putStr "\ESC[2J" 

hideCur :: IO ()
hideCur = putStr "\ESC[?25l"

showCur :: IO ()
showCur = putStr "\ESC[?25h"

chColor1 :: IO ()
chColor1 = putStr "\ESC[36m"

chColor2 :: IO ()
chColor2 = putStr "\ESC[31m"

chColor3 :: IO ()
chColor3 = putStr "\ESC[33m"

mkDefault :: IO ()
mkDefault = putStr "\ESC[39m"

getMozi :: IO Char
getMozi = getHiddenChar
