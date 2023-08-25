{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random(randomRIO, randomIO)
import System.Environment(getArgs)
import qualified Data.Text as T
import Yokotex(makeTex)

type State = [(Int,Int)]

toTx :: Int -> T.Text
toTx i = T.pack$show i

question0 :: State -> Int -> IO ((T.Text,T.Text),State)
question0 st n = do 
  a <- randomRIO (0,9)
  b <- randomRIO (0,9)
  if (a,b) `elem` st then question0 st n else do
                        let tx = "□  " <> toTx a <> " × " <> toTx b <> " = "
                            nst = if length st > 90 then [] else (a,b):st
                            ans ="■  " <> toTx (a*b)
                        return ((tx,ans),nst)

question1 :: State -> Int -> IO ((T.Text,T.Text),State)
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
                            ans = "■  " <> toTx (a+b)
                        return ((tx,ans),nst)

makeDoc :: Int -> Int -> IO (T.Text,T.Text)
makeDoc t n = do
  let qpl = case t of 0 -> 3; 1 -> 3; _ -> 1
  (q,a) <- makeDoc' [] 1 t n
  let qtx = "\\begin{multicols}{"<>toTx qpl<>"}\n"<>q<>"\\end{multicols}"
      atx = "\\begin{multicols}{"<>toTx qpl<>"}\n"<>a<>"\\end{multicols}"
  return (qtx,atx)

makeDoc' :: State -> Int -> Int -> Int -> IO (T.Text,T.Text)
makeDoc' st i t n
  | n < i = return (T.empty,T.empty)
  | otherwise = do
    ((q,a),nst) <- case t of 
                  0 -> question0 st i
                  1 -> question1 st i
                  _ -> return ((T.empty,T.empty),st)
    lns <- makeDoc' nst (i+1) t n
    return (q <> "\n" <> fst lns,a <> "\n" <> snd lns)

makeHead :: Bool -> Int -> Int -> T.Text
makeHead b t n =
  let hd0 = case t of
              0 -> "\\scriptsize かけ算 1桁ー"
              1 -> "\\scriptsize 正負二項計算ー"
              _ -> ""
      hd = if b then "\n\\lhead{"<>hd0<>"練習}\n"
                else "\n\\lhead{"<>hd0<>"解答}\n"
   in hd

main :: IO ()
main = do
  arg <- getArgs
  makeTex "math" arg makeDoc makeHead
