{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random(randomRIO, randomIO)
import System.Environment(getArgs)
import qualified Data.Text as T
import Data.Ratio ((%),numerator,denominator)
import Yokotex(makeTex)

type State = [(Int,Int)]
type Questions = T.Text
type Answers = T.Text
type Doc = (T.Text,T.Text)

toTx :: Int -> T.Text
toTx i = T.pack$show i

question0 :: State -> [Int] -> Int -> IO (Doc,State)
question0 st te n = do 
  let (mi,ma) = if length te < 2 then (0,9) else (head te,head$tail te) 
  a <- randomRIO (mi,ma)
  b <- randomRIO (mi,ma)
  if (a,b) `elem` st then question0 st te n else do
    let txa = if a<0 then "- "<>toTx (abs b) else toTx a 
        txb = if b<0 then "(- "<>toTx (abs b)<>")" else toTx b
        tx = "□  " <> txa <> " × " <> txb <> " = "
        nst = if length st > 90 then [] else (a,b):st
        ans ="■  " <> toTx (a*b)
    return ((tx,ans),nst)

question1 :: State -> [Int] -> Int -> IO (Doc,State)
question1 st te n = do
  let (mi,ma) = if length te < 2 then (-20,20) else (head te,head$tail te) 
  a <- randomRIO (mi,ma)
  b <- randomRIO (mi,ma)
  if (a,b) `elem` st || a==0 || b ==0 then question1 st te n else do
                        let txa = if a<0 then "- "<>toTx (abs a) else toTx a
                            txb 
                              | b>0 = "+ "<>toTx b
                              | otherwise = "- "<>toTx (abs b)
                            tx = "□  " <> txa <> " " <> txb <> " = "
                            nst = if length st > 90 then [] else (a,b):st
                            ans = "■  " <> toTx (a+b)
                        return ((tx,ans),nst)

question2 :: State -> Int -> IO (Doc,State)
question2 st n = do
  a <- randomRIO (-9,9)
  p <- randomRIO (0,1::Int)
  dn <- randomRIO (2,15)
  nm <- randomRIO (-(dn-1),dn-1)
  if (a,dn) `elem` st || a==0 || nm==0 then question2 st n else do
    let txa 
          | a<0 = "- "<>toTx (abs a) 
          | p>0 = "+ "<>toTx a
          | otherwise = toTx a
        txb
          | nm<0 = "- "<>"\\frac{"<>toTx (abs nm)<>"}{"<>toTx dn<>"}" 
          | p==0 = "+ "<>"\\frac{"<>toTx nm<>"}{"<>toTx dn<>"}"
          | otherwise = "\\frac{"<>toTx nm<>"}{"<>toTx dn<>"}"
        tx = "$"<>"□  "<>(if p==0 then txa<>" "<>txb else txb<>" "<>txa)<>" = "<>"$"
        nst = if length st > 90 then [] else (a,dn):st
        res = (a%1) + (nm%dn)
        rnm = numerator res
        rdn = denominator res
        ans = "$"<>"■  "<>(if rnm<0 then "-" else "")<>"\\frac{"<>toTx (abs rnm)<>"}{"<>toTx rdn<>"}"<>"$"
    return ((tx,ans),nst)

makeDoc :: Int -> Int -> [Int] -> IO Doc 
makeDoc t n te = do
  let qpl = case t of 0 -> 3; 1 -> 3; 2 -> 2; _ -> 1
  (q,a) <- makeDoc' [] 1 t n te
  let qtx = "\\begin{multicols}{"<>toTx qpl<>"}\n"<>q<>"\\end{multicols}"
      atx = "\\begin{multicols}{"<>toTx qpl<>"}\n"<>a<>"\\end{multicols}"
  return (qtx,atx)

makeDoc' :: State -> Int -> Int -> Int -> [Int] -> IO Doc 
makeDoc' st i t n te
  | n < i = return (T.empty,T.empty)
  | otherwise = do
    ((q,a),nst) <- case t of 
                  0 -> question0 st te i
                  1 -> question1 st te i
                  2 -> question2 st i
                  _ -> return ((T.empty,T.empty),st)
    lns <- makeDoc' nst (i+1) t n te
    return (q <> "\n" <> fst lns,a <> "\n" <> snd lns)

makeHead :: Bool -> Int -> Int -> [Int] -> T.Text
makeHead b t n te =
  let hd0 = case t of
              0 -> "\\scriptsize かけ算 1桁ー"
              1 -> "\\scriptsize 正負二項計算ー"
              2 -> "\\scriptsize 正負二項計算 一項分数＿"
              _ -> ""
      hd = if b then "\n\\lhead{"<>hd0<>"練習}\n"
                else "\n\\lhead{"<>hd0<>"解答}\n"
   in hd

main :: IO ()
main = do
  arg <- getArgs
  makeTex "math" arg makeDoc makeHead
