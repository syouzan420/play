{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random(randomRIO, randomIO)
import System.Environment(getArgs)
import qualified Data.Text as T
import Data.Ratio ((%),numerator,denominator)
import Control.Monad (replicateM)
import Data.List (findIndices)
import Data.Functor ((<&>))
import Yokotex(makeTex)
import Equation(solveEquation)
import Syosu(showsu)

type State = [(Int,Int)]
type Ext = [Int] -- extension
type Questions = T.Text
type Answers = T.Text
type Doc = (T.Text,T.Text)

toTx :: Int -> T.Text
toTx i = T.pack$show i

setEx :: Ext -> Ext -> Int -> Int
setEx ex dex n = let lex=length ex in if lex>n && ex!!n/=0 then ex!!n else dex!!n

def4 :: [(Int,Int,Int)]
def4 = [(40,50,100),(15,200,300)]

-- t1m1f1 項数1 かけ算1 分数1 のやうに指定したい
-- 例へば50問中10問がランダムで 残りは指定した問題を順番・もしくはランダムで出題したい
question4 :: State -> Ext -> Int -> IO (Doc,State)
question4 st ex n = do
  --ex: 0: max0, 1: max1, 2: max2, 3: max3, 4: whole, 5: part, 6: random(0) first-frac(1) second-frac(2) 
  --ex: 7: defined(0), make one to 100(1), random(2)
  let dfEx = [10,10,10,10,30,3,0,2]
      lex = length ex
      lastSpace = if n `mod` 10 == 0 then "" else "\\\\"
      lastSpaceAns 
        | n `mod` 20 == 0 = "\\newpage" 
        | n `mod` 10 == 0 = "\\vfil\\null\\columnbreak" 
        | otherwise = "\\\\"
      [mx0,mx1,mx2,mx3,wl,pt,tp,mh] = map (setEx ex dfEx) [0..7]
  [frc,mln,mln2,mld,ndef,lor,uh,wih,di] 
        <- mapM randomRIO [(2,mx0),(2,mx1),(2,mx2),(2,mx3),(1,wl),(0,1),(0,1),(0,2),(0,length def4 - 1)]
  let idef = ndef <= pt  -- 定義された値を使ふかどうか
      ifst = lor == 0    -- 分数を最初にするかどうか
      imoh = mh == 1 || (mh == 2 && uh == 1)    -- どれかひとつを100にするかどうか
      (num0,num1,num2) = if idef then def4!!di else (frc*mln,frc*mln2,frc*mld)
      (n0,n1,n2) = if imoh then 
                    case wih of 0 -> (100,num1,num2); 1 -> (num0,100,num2); 2 -> (num0,num1,100)
                           else (num0,num1,num2)
  if (n0,n1) `elem` st || n0==n1 || n0==n2 || n1==n2 then question4 st ex n else do
    let txa = "\\dfrac{"<>toTx n0<>"}{"<>toTx n1<>"}" 
        txb = toTx n2
        res = (n0*n2)%n1
        rnu = numerator res
        rde = denominator res
        isInt = rde == 1
        syo = if isInt then "" else showsu (fromIntegral rnu / fromIntegral rde) 1 
        nst = if length st > (mx1-1)*(mx2-1)*(mx3-1) then [] else (n0,n1):st
        tx = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>(if ifst then txa<>" \\times "<>txb else txb<>" \\times "<>txa)<>" = "<>"$"<>lastSpace
        ans = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>(if isInt then toTx rnu else "\\dfrac{"<>toTx rnu<>"}{"<>toTx rde<>"}")<>"\\hspace{1em}"<>(if isInt then "" else "( "<>T.pack syo<>" )")<>"$"<>lastSpaceAns
    return ((tx,ans),nst)


-- かけ算 a x b
question0 :: State -> Ext -> Int -> IO (Doc,State)
question0 st ex n = do 
  -- ex!!0: minNumber, ex!!1: maxNumber, ex!!2: 0-random 1-in order, ex!!3: 1--min&max only affect left term
  let (mi,ma) = minma (1,9) ex 
      ior = if length ex < 3 then 0 else ex!!2
      ima = if length ex < 4 then 0 else ex!!3
      oa = (n-1) `div` 9 + mi 
      ob = (n-1) `mod` 9 + 1
      (mi2,ma2) = if ima==0 then (mi,ma) else (1,9)
  ra <- randomRIO (mi,ma)
  rb <- randomRIO (mi2,ma2)
  let a = if ior==0 then ra else oa 
      b = if ior==0 then rb else ob
  if (a,b) `elem` st then question0 st ex n else do
    let txa = if a<0 then "- "<>toTx (abs a) else toTx a 
        txb = if b<0 then "(- "<>toTx (abs b)<>")" else toTx b
        tx = "□  " <> txa <> " × " <> txb <> " = "
        nst = if length st > (ma-mi+1)*(ma2-mi2) then [] else (a,b):st
        ans ="■  " <> toTx (a*b)
    return ((tx,ans),nst)

minusPositions :: [Int] -> [Int]
minusPositions tms = findIndices (<0) (init tms)
--minusPositions tms = foldl (\acc (n,i) -> if n<0 then acc++[i] else acc) [] (zip (init tms) [0..]) 

decideParPos :: Int -> [Int] -> IO [(Int,Int)]
decideParPos lng mps = do         --lng: term length, mps: minus positions
  a <- randomRIO (0,length mps)
  if a==0 then return []
          else do
            let lpInd = mps!!(a-1)
            rpInd <- randomRIO (lpInd+1,lng-1)
            let nmps = map (\x -> x-rpInd-1) $ filter (>rpInd) mps
                nlng = lng - rpInd - 1
            pps <- decideParPos nlng nmps
            return ((lpInd,rpInd):pps)

parPos :: Int -> [(Int,Int)] -> ([Int],[Int])
parPos _ [] = ([],[])
parPos n ((l,r):xs) = let ls = fst$parPos (r+1) xs
                          rs = snd$parPos (r+1) xs
                       in (l+n:ls,r+n:rs) 

exTerms :: [Int] -> ([Int],[Int]) -> [Int]
exTerms tms ([],[]) = tms 
exTerms tms (l:ls,r:rs) =
  let ntms = zipWith (\n i -> if i>l && i<=r then (-n) else n) tms [0..] 
   in exTerms ntms (ls,rs)

minma :: (Int,Int) -> Ext -> (Int,Int)
minma df ex = let defaultValue = df 
                  mm = if length ex < 2 then defaultValue else (head ex,head$tail ex)
               in if mm == (0,0) then defaultValue else mm

question1 :: State -> Ext -> Int -> IO (Doc,State)
question1 st ex n = do
  -- ex!!0: minNumber, ex!!1: maxNumber, ex!!2 numberOfTerms, ex!!3 numberOfParenthesis
  let (mi,ma) = minma (-9,9) ex
      nt = if length ex < 3 then 2 else ex!!2
      iskt = length ex > 3 && ex!!3>0
  tms <- replicateM nt $ randomRIO (mi,ma) 
  (lps,rps) <- decideParPos (length tms) (minusPositions tms) <&> parPos 0
  let hd = head tms
  if (hd,tms!!1) `elem` st || elem 0 tms then question1 st ex n else do
    let txh = if hd<0 then "-- "<>(if iskt&&not (null lps)&&(head lps==0) then "(" else T.empty)
                                <>toTx (abs hd) else toTx hd
        txs = zipWith (\n i -> if n>0 then "+ "<>toTx n<>(if iskt&&elem i rps then ")" else T.empty)
                                      else "-- "<>(if iskt&&elem i lps then "(" else T.empty)
                                                <>toTx (abs n)
                                                <>(if iskt&&elem i rps then ")" else T.empty))
                                                      (tail tms) [1..]
        tx = foldl (\acc t -> acc <> " " <> t) 
                      ("\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>txh) txs <> " = "
        nst = if length st > (ma-mi)*(ma-mi-1) then [] else (hd,tms!!1):st
        ntms = if iskt then exTerms tms (lps,rps) else tms
        ans = "\\tiny"<>T.pack (show n)<>"\\normalsize □  " <> toTx (sum ntms)
    return ((tx,ans),nst)

question2 :: State -> Ext -> Int -> IO (Doc,State)
question2 st ex n = do
  -- ex!!0: minNumber, ex!!1: maxNumber, ex!!2: numberOfFractionTerms 
  let (mi,ma) = minma (2,15) ex
      nf = if length ex < 3 then 1 else ex!!2
      lastSpace = if n `mod` 10 == 0 then "" else "\\\\"
  a <- randomRIO (-9,9)
  p <- randomRIO (0,1::Int)
  dn <- randomRIO (mi,ma)
  nm <- randomRIO (-(dn-1),dn-1)
  dn2 <- randomRIO (mi,ma)
  nm2 <- randomRIO (-(dn-1),dn-1)
  case nf of
    1 -> if (a,dn) `elem` st || a==0 || nm==0 then question2 st ex n else do
          let txa 
                | a<0 = "- "<>toTx (abs a) 
                | p>0 = "+ "<>toTx a
                | otherwise = toTx a
              txb
                | nm<0 = "- "<>"\\dfrac{"<>toTx (abs nm)<>"}{"<>toTx dn<>"}" 
                | p==0 = "+ "<>"\\dfrac{"<>toTx nm<>"}{"<>toTx dn<>"}"
                | otherwise = "\\dfrac{"<>toTx nm<>"}{"<>toTx dn<>"}"
              tx = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>(if p==0 then txa<>" "<>txb else txb<>" "<>txa)<>" = "<>"$"<>lastSpace
              nst = if length st > 90 then [] else (a,dn):st
              res = (a%1) + (nm%dn)
              rnm = numerator res
              rdn = denominator res
              ans = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>(if rnm<0 then "-" else "")<>"\\frac{"<>toTx (abs rnm)<>"}{"<>toTx rdn<>"}"<>"$"<>"\\\\"
          return ((tx,ans),nst)
    _ -> if (dn,dn2) `elem` st || nm==0 || nm2==0 then question2 st ex n else do
          let txa 
                | nm2<0 = "- "<>"\\dfrac{"<>toTx (abs nm2)<>"}{"<>toTx dn2<>"}" 
                | otherwise = "\\dfrac{"<>toTx nm2<>"}{"<>toTx dn2<>"}"
              txb
                | nm<0 = "- "<>"\\dfrac{"<>toTx (abs nm)<>"}{"<>toTx dn<>"}" 
                | otherwise = "+ "<>"\\dfrac{"<>toTx nm<>"}{"<>toTx dn<>"}"
              tx = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>txa<>" "<>txb<>" = "<>"$"<>lastSpace
              nst = if length st > 90 then [] else (dn,dn2):st
              res = (nm2%dn2) + (nm%dn)
              rnm = numerator res
              rdn = denominator res
              isInt = rdn==1
              ans = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>(if rnm<0 then "-" else "")<>(if isInt then toTx (abs rnm) else "\\frac{"<>toTx (abs rnm)<>"}{"<>toTx rdn<>"}")<>"$"<>"\\\\"
          return ((tx,ans),nst)

intoTex :: Bool -> Bool -> Int -> T.Text
intoTex b b2 i -- b:int is in first term, b2: int is in front of a variable
  | i<0 = "- "<>if b2 && (abs i==1) then T.empty else toTx (abs i)
  | b = if b2 && (abs i==1) then if i<0 then "-" else T.empty else toTx i
  | otherwise = "+ "<>if b2 && (abs i==1) then T.empty else toTx i

question3 :: State -> Ext -> Int -> IO (Doc,State)
question3 st ex n = do 
--  let (mi,ma) = if length ex < 2 then (0,9) else (head ex,head$tail ex) 
  let (mi,ma) = minma (-9,9) ex 
  a <- randomRIO (mi,ma)
  b <- randomRIO (mi,ma)
  c <- randomRIO (mi,ma)
  d <- randomRIO (mi,ma)
  e <- randomRIO (0::Int,3)
  let v = "x"
  if (a,b) `elem` st || a==0 || b==0 || c==0 || d==0 
                     || (e==0 && a==c) || (e==1 && a==d) || (e==2 && b==c) || (e==3 && b==d) 
      then question3 st ex n else do
    let txa = if a<0 then "- "<>toTx (abs a) else toTx a 
        txb = if b<0 then "(- "<>toTx (abs b)<>")" else toTx b
        lexp = case e of
                  0 -> intoTex True True a <>v<>" "<>intoTex False False b
                  1 -> intoTex True True a <>v<>" "<>intoTex False False b
                  2 -> intoTex True False a <>" "<>intoTex False True b<>v
                  3 -> intoTex True False a <>" "<>intoTex False True b<>v
        rexp = case e of
                  0 -> intoTex True True c <>v<>" "<>intoTex False False d
                  1 -> intoTex True False c <>" "<>intoTex False True d<>v
                  2 -> intoTex True True c <>v<>" "<>intoTex False False d
                  3 -> intoTex True False c <>" "<>intoTex False True d<>v
        tx = "\\tiny"<>T.pack (show n)<>"\\normalsize □  $" <> lexp <> " = " <> rexp <> "$\\\\\\\\\\\\"
        nst = if length st > (ma-mi)*(ma-mi-1) then [] else (a,b):st
        res = solveEquation (T.unpack (lexp<>"="<>rexp)) (T.head v)
        rnm = numerator res
        rdn = denominator res
        isInt = rdn==1
        ans = "\\tiny"<>T.pack (show n)<>"\\normalsize □  "<>"$"<>(if rnm<0 then "-" else "")<>(if isInt then toTx (abs rnm) else "\\frac{"<>toTx (abs rnm)<>"}{"<>toTx rdn<>"}")<>"$"<>"\\"
    return ((tx,ans),nst)


makeDoc :: Int -> Int -> Ext -> IO Doc 
makeDoc t n ex = do
  let qpl = case t of 0 -> 3
                      1 -> if length ex < 3 then 3 else if ex!!2<5 then 2 else 1
                      2 -> 2
                      3 -> 2
                      4 -> 2
                      _ -> 1
  (q,a) <- makeDoc' [] 1 t n ex 
  let qtx = if qpl==1 then q else "\\begin{multicols}{"<>toTx qpl<>"}\n"<>q<>"\\end{multicols}"
      atx = if qpl==1 then a else "\\begin{multicols}{"<>toTx qpl<>"}\n"<>a<>"\\end{multicols}"
  return (qtx,atx)

makeDoc' :: State -> Int -> Int -> Int -> Ext -> IO Doc 
makeDoc' st i t n ex 
  | n < i = return (T.empty,T.empty)
  | otherwise = do
    ((q,a),nst) <- case t of 
                  0 -> question0 st ex i
                  1 -> question1 st ex i
                  2 -> question2 st ex i
                  3 -> question3 st ex i
                  4 -> question4 st ex i
                  _ -> return ((T.empty,T.empty),st)
    lns <- makeDoc' nst (i+1) t n ex
    return (q <> "\n" <> fst lns,a <> "\n" <> snd lns)

makeHead :: Bool -> Int -> Int -> Ext -> T.Text
makeHead b t n ex =
  let tms = case t of
              1 -> if length ex < 3 then 2 else ex!!2
              2 -> if length ex < 3 then 1 else ex!!2
              _ -> 0 
      hd0 = case t of
              0 -> "\\scriptsize かけ算 1桁----"
              1 -> "\\scriptsize 正負"<>T.pack (show tms)<>"項計算----"
              2 -> "\\scriptsize 正負2項計算 "<>T.pack (show tms)<>"項分数----"
              3 -> "\\scriptsize 方程式各辺2項---- "
              4 -> "\\scriptsize 分数×自然数----答えが分数の場合 さらに分子を分母で割る (少数第一位まで)\\\\(第二位を四捨五入)------"
              _ -> ""
      hd = if b then "\n\\lhead{"<>hd0<>"\\small 練習}\n"
                else "\n\\lhead{"<>hd0<>"\\small 解答}\n"
   in hd

main :: IO ()
main = do
  arg <- getArgs
  makeTex "math" arg makeDoc makeHead
