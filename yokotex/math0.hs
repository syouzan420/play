{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random(randomRIO, randomIO)
import System.Environment(getArgs)
import qualified Data.Text as T
import Yokotex(makeTex)

type State = [(Int,Int)]

toTx :: Int -> T.Text
toTx i = T.pack$show i

question0 :: State -> Int -> IO (T.Text,State)
question0 st n = do 
  a <- randomRIO (0,9)
  b <- randomRIO (0,9)
  if (a,b) `elem` st then question0 st n else do
                        let tx = "□  " <> toTx a <> " × " <> toTx b <> " = "
                            nst = if length st > 90 then [] else (a,b):st
                        return (tx,nst)

question1 :: State -> Int -> IO (T.Text,State)
question1 st n = do
  a <- randomRIO (-20,20)
  b <- randomRIO (-20,20)
  if (a,b) `elem` st || a==0 || b ==0 then question1 st n else do
                        let txa = if a<0 then "- "<>toTx (abs a) else toTx a
                            txb 
                              | b>0 = "+ "<>toTx b
                              | otherwise = "- "<>toTx (abs b)
                            tx = "□  " <> txa <> " " <> txb <> " = "
                            nst = if length st > 90 then [] else (a,b):st
                        return (tx,nst)

makeDoc :: Int -> Int -> IO T.Text
makeDoc = makeDoc' [] 1

makeDoc' :: State -> Int -> Int -> Int -> IO T.Text
makeDoc' st i t n
  | n < i = return T.empty
  | otherwise = do
    let qpl = case t of 0 -> 3; 1 -> 3; _ -> 1
    (ln,nst) <- makeLine st t i qpl
    lns <- makeDoc' nst (i+qpl) t n
    return (ln <> "\n" <> lns)

makeLine :: State -> Int -> Int -> Int -> IO (T.Text,State)
makeLine st _ _ 0 = return (T.empty,st)
makeLine st t i qpl = do
  (q,nst) <- case t of
    0 -> question0 st i 
    1 -> question1 st i
    _ -> return (T.empty,st) 
  let sp = case t of
             0 -> "    "
             1 -> "   "
             _ -> "   "
  qs <- makeLine nst t (i+1) (qpl-1)
  return (q <> sp <> fst qs,snd qs)

makeHead :: Int -> Int -> T.Text
makeHead t n =
  let hd0 = case t of
              0 -> "\\scriptsize かけ算 1桁ー"
              1 -> "\\scriptsize 正負二項計算ー"
              _ -> ""
      hd = "\n\\lhead{"<>hd0<>"練習}\n"
   in hd

main :: IO ()
main = do
  arg <- getArgs
  makeTex "math0" arg makeDoc makeHead