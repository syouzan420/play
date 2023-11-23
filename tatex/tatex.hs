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
  ,"twocolumn,"
  ,"paper=a4,"
  ,"fontsize=16pt,"
  ,"jafontsize=14pt,"
  ,"number_of_lines=22,"
  ,"line_length=25zh,"
  ,"baselineskip=21pt,"
  ,"]{jlreq}"
  ,"\\usepackage{pxrubrica}"
  ,"\\usepackage[utf8]{inputenc}"
  ,"\\usepackage{pxfonts}"
  ,"\\usepackage[T1]{fontenc}"
  ,"\\author{yokoP}"
  ,"\\title{title}"
  ,"\\usepackage{fancyhdr}"
  ]

strToLatex :: String -> String
strToLatex str =
  let lns = lines str
      nlns = map cnvRubi lns
      res = listToLatex nlns 
   in unlines res

strToDen :: String -> String
strToDen str =
  let lns = lines str
      res = map (cnvRubiD.kakkoD) (kaigyoD lns) 
   in unlines res

listToLatex :: [String] -> [String]
listToLatex = map (++"\\\\") 

cnvSpace :: String -> String
cnvSpace str = 
  let str2 = if  "     " `isInfixOf` str then replace "     " "\\hspace{3em}" str else str
      str3 = if  "   " `isInfixOf` str2 then replace "   " "\\hspace{2em}" str2 else str2
      str4 = if " " `isInfixOf` str3 then replace " " "\\space" str3 else str3
   in str4 

cnvRubi :: String -> String 
cnvRubi [] = []
cnvRubi (x:'：':xs)
  | '：' `elem` xs =
    let (hd,tl) = break (=='：') xs
        lhd = length hd
     in if lhd < 8 then "\\ruby{"++[x]++"}{"++hd++"} "++cnvRubi (tail tl)
                   else [x]++"："++cnvRubi xs
  | otherwise = x:cnvRubi xs
cnvRubi (x:xs) = x:cnvRubi xs

cnvRubiD :: String -> String 
cnvRubiD [] = []
cnvRubiD (x:'：':xs)
  | '：' `elem` xs =
    let (hd,tl) = break (=='：') xs
        lhd = length hd
     in if lhd < 8 then "{"++[x]++"|"++hd++"} "++cnvRubiD (tail tl)
                   else [x]++"："++cnvRubiD xs
  | otherwise = x:cnvRubiD xs
cnvRubiD (x:xs) = x:cnvRubiD xs

kaigyoD :: [String] -> [String]
kaigyoD [] = [] 
kaigyoD (x:xs) 
  | x=="" = "<p><br /></p>" : kaigyoD xs
  | otherwise = x : kaigyoD xs

kakkoD :: String -> String
kakkoD [] = []
kakkoD (x:xs)
  | x=='（' = " ("++kakkoD xs
  | x=='）' = ") "++kakkoD xs
  | otherwise = x : kakkoD xs


main :: IO ()
main = do
  arg <- getArgs
  let fileName = if null arg then "" else head arg
  a <- fileRead fileName 
  let la = latexHeader ++ "\\begin{document}\n" ++ strToLatex a ++ "\\end{document}"
      dn = strToDen a
  fileWrite (fileName++".tex") la
  fileWrite ("den_"++fileName) dn 
  return ()

