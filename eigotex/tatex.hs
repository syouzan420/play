module Main where

import System.Environment(getArgs)
--import qualified Data.Text as T
import Data.List(isInfixOf)
import Data.List.Utils(replace)
import Myfile(fileWrite,fileRead)

latexHeader :: String
latexHeader = unlines
  ["\\RequirePackage{plautopatch}"
  ,"\\documentclass[uplatex,"
  ,"tate,"
  ,"paper=a4,"
  ,"fontsize=16pt,"
  ,"jafontsize=14pt,"
  ,"number_of_lines=30,"
  ,"line_length=30zh,"
  ,"baselineskip=25pt,"
  ,"]{jlreq}"
  ,"\\usepackage{pxrubrica}"
  ,"\\usepackage[utf8]{inputenc}"
  ,"\\usepackage{pxfonts}"
  ,"\\usepackage[T1]{fontenc}"
  ,"\\author{yokoP}"
  ,"\\title{title}"
  ,"\\usepackage{fancyhdr}"
  ,"\\pagestyle{fancy}"
  ,"\\rhead{\\scriptsize\\space\\today\\space\\normalsize\\textgt{☆}P\\thepage}"
  ]

strToLatex :: String -> String
strToLatex str =
  let lns = lines str
      nlns = map cnvSpace lns
      res = listToLatex nlns
   in unlines res

listToLatex :: [String] -> [String]
listToLatex = map (++"\\\\") 

cnvSpace :: String -> String
cnvSpace str = 
  let str2 = if  "     " `isInfixOf` str then replace "     " "\\hspace{3em}" str else str
      str3 = if  "   " `isInfixOf` str2 then replace "   " "\\hspace{2em}" str2 else str2
      str4 = if " " `isInfixOf` str3 then replace " " "\\space" str3 else str3
   in str4 

main :: IO ()
main = do
  arg <- getArgs
  let fileName = if null arg then "" else head arg
  a <- fileRead fileName 
  let la = latexHeader ++ "\\begin{document}\n" ++ strToLatex a ++ "\\end{document}"
  fileWrite (fileName++".tex") la
  return ()

