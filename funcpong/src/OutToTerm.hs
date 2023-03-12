{-# LANGUAGE OverloadedStrings #-}
module OutToTerm (putMessage,putGrid,putMoziLn,putMozi) where

import System.IO (hFlush,stdout)
import Define (ix,Msg,Grid,Pos,Mode(DB,DF))
import Terminal (getMozi,cls)

goto :: Pos -> IO ()
goto (x,y) = do 
  putStr ("\ESC["++show y++";"++show x++"H")
  hFlush stdout

putMessage :: Pos -> Msg -> IO ()
putMessage _ [] = return ()
putMessage (p,q) (x:xs) = do
  goto (p,q) 
  putStrLn x
  getMozi 
  putMessage (p,q+1) xs

putGrid :: Pos -> Grid -> IO ()
putGrid (x,y) grid = do 
  cls
  goto (x,y)
  putStrLn edgeLine
  putInside grid
    where gridWidth = length (head grid)
          edgeLine = ("-"++(replicate gridWidth '-')++"-")
          leftSpace = replicate (ix-1) ' '
          putInside [] = putStrLn (leftSpace++edgeLine)
          putInside (g:gs) = do
            let inside = map (\(ch,tp) -> if(tp==DB || tp==DF) then ' ' else ch) g
            putStrLn (leftSpace++"|"++inside++"|")
            putInside gs

putMoziLn :: Pos -> String -> IO ()
putMoziLn pos str = goto pos >> putStrLn str >> hFlush stdout

putMozi :: Pos -> String -> IO ()
putMozi pos str = goto pos >> putStr str >> hFlush stdout
