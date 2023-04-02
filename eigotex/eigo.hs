import System.Random (randomRIO)
import qualified Data.Map.Strict as M
import Data.List(isInfixOf)
import Data.List.Utils(replace)
import Myfile(fileWrite)

data Be = Am | Are | Is deriving Eq
data Wtype =  To | Fr | Th | Fo | Pl | Pa  deriving Eq
-- To, From, Thing, Food, Place, Parson
data Jtype = Wo | Ni | Kr | P Int deriving Eq
data Nc = Cn | Un deriving Eq -- Countable, Uncountable

type Subject = String
type JpSubject = String
type Verb = String
type VerbS = String
type JpVerb = String
type Noun = String
type JpNoun = String
type Athe = String

subB :: M.Map Subject Be
subB = M.fromList 
  [("I",Am),("You",Are),("We",Are),("He",Is),("She",Is),("They",Are),("It",Is)]

subJ :: M.Map JpSubject Subject 
subJ = M.fromList 
  [("私は","I"),("あなたは","You"),("あなたたちは","You"),("私たちは","We")
  ,("彼は","He"),("彼女は","She"),("彼らは","They")]

verbNow :: M.Map Verb VerbS 
verbNow = M.fromList 
  [("give","gives"),("take","takes"),("buy","buys"),("eat","eats"),("see","sees")
  ,("go","goes"),("come","comes")]

verbJ :: M.Map JpVerb (Verb,[Wtype],[Jtype])
verbJ = M.fromList
  [("あげる",("give",[Th,To,Pa],[P 0,Wo,P 2,Ni]))
  ,("もらう",("take",[Th,Fr,Pa],[P 2,Kr,P 0,Wo]))
  ,("つれていく",("take",[Pa,To,Pl],[P 0,Wo,P 2,Ni]))
  ,("買う",("buy",[Th],[P 0,Wo]))
  ,("食べる",("eat",[Fo],[P 0,Wo]))
  ,("見る",("see",[Th],[P 0,Wo]))
  ,("行く",("go",[To,Pl],[P 1,Ni]))
  ,("来る",("come",[To,Pl],[P 1,Ni]))]

nounC :: M.Map Noun Athe 
nounC = M.fromList
  [("apple","an"),("cake","a"),("pencil","a"),("note","a"),("school",""),("library","the"),("Tom","")]

nounT :: M.Map Noun Wtype
nounT = M.fromList
  [("apple",Fo),("cake",Fo),("pencil",Th),("note",Th),("school",Pl),("library",Pl),("Tom",Pa)]

nounJ :: M.Map Noun JpNoun 
nounJ = M.fromList
  [("apple","りんご"),("cake","ケーキ"),("pencil","鉛筆"),("note","ノート")
  ,("school","学校"),("library","図書館"),("Tom","トム")]

getRand :: Int -> IO Int
getRand i = randomRIO (0,i-1)

makeSentence :: Int -> M.Map JpSubject Subject -> M.Map JpVerb (Verb,[Wtype],[Jtype]) -> IO (String,String) 
makeSentence 0 _ _ = return ("","")
makeSentence i sujL verbL = do
  let subSize = M.size sujL  
  sr <- getRand subSize
  let (jsub,esub) = M.elemAt sr sujL
      nsujL = M.deleteAt sr sujL
      Just be = M.lookup esub subB
      verSize = M.size verbL
  vr <- getRand verSize
  let (jverb,(everb,verbteL,verbtjL)) = M.elemAt vr verbL
      nverbL = M.deleteAt vr verbL
      Just verbN = if be==Is then M.lookup everb verbNow else Just everb
  tseL <- mapM typeToString verbteL
  let tsjL = map (jtypeToString tseL) verbtjL
      eresL = esub : verbN : tseL 
      jresL = putChars '[' ']' jsub : tsjL ++ [putChars '<' '>' jverb] 
      qresL = "[     ]" : "<     >" : tseL
      nsujL' = if nsujL==M.empty then subJ else nsujL 
      nverbL' = if nverbL==M.empty then verbJ else nverbL
  (newQuestion,newAnswer) <- makeSentence (i-1) nsujL' nverbL'
  let question = (show i ++ ".  " ++ unwords jresL) ++ "\n" ++ ("  " ++ unwords qresL ++ ".")
      answer = show i ++ ".  " ++ unwords eresL ++ "." 
  --putStrLn question
  --putStrLn answer
  return (newQuestion ++ question ++ "\n", newAnswer ++ answer ++ "\n")

putChars :: Char -> Char -> String -> String
putChars h l str = h:str++[l]

jtypeToString :: [String] -> Jtype -> String
jtypeToString tL jt =
  case jt of
    Wo -> "を"
    Ni -> "に"
    Kr -> "から"
    P i -> let wd = tL!!i
               Just jw = M.lookup (last (words wd)) nounJ 
            in jw

typeToString :: Wtype -> IO String
typeToString wt =
  case wt of
    To -> return "to"
    Fr -> return "from"
    Th -> typeToNoun Th 
    Fo -> typeToNoun Fo 
    Pl -> typeToNoun Pl 
    Pa -> typeToNoun Pa 

typeToNoun :: Wtype -> IO String
typeToNoun t = do
    let thsL = M.filter (==t) nounT
        thsSize = M.size thsL
    tr <- getRand thsSize
    let noun = fst$M.elemAt tr thsL 
        Just athe = M.lookup noun nounC
        res = if athe=="" then noun else athe++" "++noun
    return res 

latexHeader :: String
latexHeader = unlines
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
  ]

strQToLatex :: String -> String
strQToLatex str =
  let lns = lines str
      nlns = map cnvSpace lns
   in unlines$listQToLatex nlns

cnvSpace :: String -> String
cnvSpace str = if "     " `isInfixOf` str then replace "     " "\\hspace{3em}" str
                                          else str

listQToLatex :: [String] -> [String]
listQToLatex [] = []
listQToLatex [x] = []
listQToLatex (x:y:xs) = x:"":y:"\\\\":"":listQToLatex xs

listAToLatex :: [String] -> [String]
listAToLatex [] = []
listAToLatex (x:xs) = x:"":listAToLatex xs

main :: IO ()
main = do
  (q,a) <- makeSentence 15 subJ verbJ
  let lq = latexHeader ++ "\\begin{document}\n" ++ strQToLatex q ++ "\\end{document}" 
      la = latexHeader ++ "\\begin{document}\n" ++ unlines (listAToLatex (lines a)) ++ "\\end{document}"
  putStrLn q
  putStrLn a
  putStrLn lq
  putStrLn la
  fileWrite "eQuestion.tex" lq
  fileWrite "eAnswer.tex" la
  return ()

