{-# LANGUAGE OverloadedStrings #-}
module Yokotex(makeTex) where

import qualified Data.Text as T
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Myfile(fileWriteT)

type FuncH = Bool->Int->Int->[Int]->T.Text
type FuncQAIO = Int->Int->[Int]->IO (T.Text,T.Text)

latexHeader :: T.Text -> T.Text 
latexHeader rid = T.unlines
  ["\\RequirePackage{plautopatch}"
  ,"\\documentclass[uplatex,"
  ,"paper=a4,"
  ,"fontsize=18pt,"
  ,"jafontsize=16pt,"
  ,"number_of_lines=30,"
  ,"line_length=30zh,"
  ,"baselineskip=25pt,"
  ,"]{jlreq}"
  ,"\\usepackage[utf8]{inputenc}"
  ,"\\usepackage{pxfonts}"
  ,"\\usepackage[T1]{fontenc}"
  ,"\\usepackage{multicol}"
  ,"\\usepackage{amsmath}"
  ,"\\author{yokoP}"
  ,"\\title{eigo}"
  ,"\\usepackage{fancyhdr}"
  ,"\\pagestyle{fancy}"
  ,"\\rhead{\\scriptsize\\space\\today\\space\\normalsize\\textgt{よこぷり☆}\\space\\textgt{"<>rid<>"}\\space p\\thepage}"
  ]

cnvSpace :: T.Text -> T.Text 
cnvSpace str = 
  let str2 = if  "     " `T.isInfixOf` str then T.replace "     " "\\hspace{3em}" str else str
      str3 = if  "   " `T.isInfixOf` str2 then T.replace "   " "\\hspace{2em}" str2 else str2
      str4 = if  "  " `T.isInfixOf` str3 then T.replace "  " "\\hspace{1em}" str3 else str3
   in str4 

strToLatex :: T.Text -> T.Text
strToLatex str = T.unlines $ listToLatex $ map cnvSpace (T.lines str)

listToLatex :: [T.Text] -> [T.Text]
listToLatex [] = []
listToLatex (x:xs) = x:"\\\\":"":listToLatex xs

makeRandomID :: IO String
makeRandomID = do replicateM 3
    $ do a <- randomRIO (0,26)
         return (toEnum (65+a) :: Char)

makePages :: [String] -> FuncQAIO -> FuncH -> (T.Text,T.Text) -> IO (T.Text,T.Text)
makePages arg f g (qq,aa) = do
  let tp = if null arg then "" else T.pack$head arg
      qn = if null arg || null (tail arg) then 40 else read$head$tail arg
      tpsp = if null arg then [] else map (\s -> if s=="" then 0 else (read.T.unpack) s) (T.split (==',') tp)
      (tp0,tpe) = if null arg then (0,[]) else (head tpsp,tail tpsp)
  (q,a) <- f tp0 qn tpe
  let hq = g True tp0 qn tpe
      ha = g False tp0 qn tpe
      nq = qq <> hq <> strToLatex q 
      na = aa <> ha <> strToLatex a
  if length arg > 2 then makePages (drop 2 arg) f g (nq<>"\n\\newpage",na<>"\n\\newpage")
                    else return (nq,na)

makeTex :: FilePath -> [String] -> FuncQAIO -> FuncH -> IO () 
makeTex fname arg f g = do
  rid <- makeRandomID
  (qs,as) <- makePages arg f g ("","")
  let ltexQ = latexHeader (T.pack rid)<> "\\begin{document}\n" <> qs <> "\\end{document}"
      ltexA = latexHeader (T.pack rid)<> "\\begin{document}\n" <> as <> "\\end{document}"
  fileWriteT (fname++"Q.tex") ltexQ
  fileWriteT (fname++"A.tex") ltexA
  return ()
