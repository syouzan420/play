module Mymain (mainApp) where

import Myaudio
import Myfile
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.List.Split(splitOn)
import Control.Monad(when)

phraseFile :: String
phraseFile = "phrases.txt"

mainApp :: IO ()
mainApp = do
  hSetBuffering stdout NoBuffering
  con <- fileRead phraseFile
  let pl = splitOn "|" (concat$lines con)
  md <- modeSelect 
  mondai md 1 pl

modeSelect :: IO Int
modeSelect = do
  putStr "Select Mode 0 or 1: "
  md <- getLine
  if md=="0" || md=="1" then return (read md) else modeSelect

mondai :: Int -> Int -> [String] -> IO ()
mondai _ _ [] = return ()
mondai _ _ [_] = return ()
mondai m i (e:j:xs) = do
  let answer = getAnswer e
      question = getQuestion e
  when (m==0) $ putStrLn j
  putStrLn$show i++". "++question
  putStr ">"
  inp <- getLine
  if inp==answer then putStrLn "Correct!"
                 else putStrLn $ "Wrong! -- "++answer
  sayWord answer
  _ <- getLine
  when (m==1) $ do putStrLn j
                   _ <- getLine
                   return ()
  mondai m (i+1) xs

getAnswer :: String -> String
getAnswer str = let target = searchAnswer$words str
                 in head$splitOn "@" target

getQuestion :: String -> String
getQuestion str = unwords$makeQuestion$words str

makeQuestion :: [String] -> [String]
makeQuestion [] = []
makeQuestion (x:xs) = if '@' `elem` x then let lst = splitOn "@" x
                                               lng = length$head lst 
                                            in ("("++replicate lng ' '++")"++(lst!!1)):makeQuestion xs
                                    else x:makeQuestion xs

searchAnswer :: [String] -> String
searchAnswer [] = []
searchAnswer (x:xs) = if '@' `elem` x then x else searchAnswer xs

