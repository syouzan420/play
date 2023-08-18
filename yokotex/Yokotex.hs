{-# LANGUAGE OverloadedStrings #-}
module Yokotex(makeTex) where

import qualified Data.Text as T
import Myfile(fileWriteT)

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
   in str3 

strToLatex :: T.Text -> T.Text
strToLatex str = T.unlines $ listToLatex $ map cnvSpace (T.lines str)

listToLatex :: [T.Text] -> [T.Text]
listToLatex (x:xs) = x:"":listToLatex xs

makePages :: [String] -> ([String]->IO T.Text) -> ([String]->T.Text) -> T.Text -> IO T.Text
makePages arg f g pp = do
  p <- f arg 
  let h = g arg
      np = p <> h <> strToLatex p 
  if length arg > 2 then makePages (drop 2 arg) f g (np<>"\n\\newpage") else return np

makeTex :: FilePath -> [String] -> ([String]->IO T.Text) -> ([String]-> T.Text) -> IO () 
makeTex fname arg f g = do
  ps <- makePages arg f g ""
  let ltex = latexHeader <> "\\begin{document}\n" <> ps <> "\\end{document}"
  fileWriteT (fname++".tex") ltex
  return ()
