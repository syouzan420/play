{-# LANGUAGE OverloadedStrings #-}
module Yokotex(makeTex) where

import qualified Data.Text as T
import Myfile(fileWriteT)

type FuncQ = Int->Int->T.Text
type FuncQIO = Int->Int->IO (T.Text,T.Text)

latexHeader :: T.Text 
latexHeader = T.unlines
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
  ,"\\author{yokoP}"
  ,"\\title{eigo}"
  ,"\\usepackage{fancyhdr}"
  ,"\\pagestyle{fancy}"
  ,"\\rhead{\\scriptsize\\space\\today\\space\\normalsize\\textgt{よこぷり☆}P\\thepage}"
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

makePages :: [String] -> FuncQIO -> FuncQ -> (T.Text,T.Text) -> IO (T.Text,T.Text)
makePages arg f g (qq,aa) = do
  let tp = if null arg then 0 else read$head arg
      qn = if null arg || null (tail arg) then 30 else read$head$tail arg
  (q,a) <- f tp qn 
  let h = g tp qn 
      nq = qq <> h <> strToLatex q 
      na = aa <> h <> strToLatex a
  if length arg > 2 then makePages (drop 2 arg) f g (nq<>"\n\\newpage",na<>"\n\\newpage")
                    else return (nq,na)

makeTex :: FilePath -> [String] -> FuncQIO -> FuncQ -> IO () 
makeTex fname arg f g = do
  (qs,as) <- makePages arg f g ("","")
  let ltexQ = latexHeader <> "\\begin{document}\n" <> qs <> "\\end{document}"
      ltexA = latexHeader <> "\\begin{document}\n" <> as <> "\\end{document}"
  fileWriteT (fname++"Q.tex") ltexQ
  fileWriteT (fname++"A.tex") ltexA
  return ()
