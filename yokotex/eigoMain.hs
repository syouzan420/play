{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Random (randomRIO)
import System.Environment(getArgs)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Data.List(isInfixOf)
import Data.List.Utils(replace)
import Data.Char(isDigit)
import Myfile(fileWrite,fileReadT,fileWriteT)

data Be = Be | Am | Are | Is deriving Eq
data WClass = S | V | C deriving Eq
data Wtype =  To | Fr | On | Th | Fo | Gr | Pl | Pa | PaO | CS | CD | CR | CT deriving Eq
-- To, From, On, Thing, Food, Greeting, Place, Parson, Parson(Objective), CanSit, CanDraw
-- CanRead, CanTell
data Jtype = Wo | Ni | Kr | T | P Int deriving Eq
-- を に から と (位置 P)

type Subject = T.Text 
type JpSubject = T.Text 
type Verb =  T.Text
type JpVerb = T.Text
type Noun = T.Text
type JpNoun = T.Text
type Athe = T.Text 
type Qtype = (Bool,Bool,Bool,Bool) -- subject, verbNow, verbPast, verbIng

subB :: M.Map Subject Be
subB = M.fromList 
  [("I",Am),("You",Are),("We",Are),("He",Is),("She",Is),("They",Are),("It",Is)]

subJ :: M.Map JpSubject Subject 
subJ = M.fromList 
  [("私は","I"),("あなたは","You"),("あなたたちは","You"),("私たちは","We")
  ,("彼は","He"),("彼女は","She"),("彼らは","They")]

verbNow :: M.Map Verb Verb 
verbNow = M.fromList 
  [("give","gives"),("take","takes"),("buy","buys"),("eat","eats"),("see","sees")
  ,("go","goes"),("come","comes"),("run","runs"),("meet","meets"),("say","says")
  ,("get","gets"),("sit","sits"),("draw","draws"),("read","reads"),("tell","tells")
  ,("have","has"),("leave","leaves")]

verbPast :: M.Map Verb Verb 
verbPast = M.fromList 
  [("give","gave"),("take","took"),("buy","bought"),("eat","ate"),("see","saw")
  ,("go","went"),("come","came"),("run","ran"),("meet","met"),("say","said")
  ,("get","got"),("sit","sat"),("draw","drew"),("read","read"),("tell","told")
  ,("have","had"),("leave","left")]

verbIng :: M.Map Verb Verb 
verbIng = M.fromList 
  [("give","giving"),("take","taking"),("buy","buying"),("eat","eating"),("see","seeing")
  ,("go","going"),("come","coming"),("run","running"),("meet","meeting"),("say","saying")
  ,("get","getting"),("sit","sitting"),("draw","drawing"),("read","reading"),("tell","telling")
  ,("have","having"),("leave","leaving")]


verbJ :: M.Map JpVerb (Verb,[Wtype],[Jtype],JpVerb,JpVerb)
verbJ = M.fromList
  [("あげる",("give",[Th,To,Pa],[P 0,Wo,P 2,Ni],"あげた","あげようとしている"))
  ,("もらう",("take",[Th,Fr,Pa],[P 2,Kr,P 0,Wo],"もらった","もらおうとしている"))
  ,("つれていく",("take",[Pa,To,Pl],[P 0,Wo,P 2,Ni],"つれていった","つれていこうとしている"))
  ,("買う",("buy",[Th],[P 0,Wo],"買った","買っているところだ"))
  ,("食べる",("eat",[Fo],[P 0,Wo],"食べた","食べているところだ"))
  ,("見る",("see",[Th],[P 0,Wo],"見た","見ているところだ"))
  ,("行く",("go",[To,Pl],[P 1,Ni],"行った","行こうとしている"))
  ,("来る",("come",[To,Pl],[P 1,Ni],"来た","来ようとしている"))
  ,("走る",("run",[],[],"走った","走っているところだ"))
  ,("会う",("meet",[Pa],[P 0,Ni],"会った","会っているところだ"))
  ,("言う",("say",[Gr,To,PaO],[P 2,Ni,P 0,T],"言った","言っているところだ"))
  ,("手に入れる",("get",[Th],[P 0,Wo],"手に入れた","手に入れているところだ"))
  ,("座る",("sit",[On,CS],[P 1,Ni],"座った","座っているところだ"))
  ,("描く",("draw",[CD],[P 0,Wo],"描いた","描いているところだ"))
  ,("読む",("read",[CR],[P 0,Wo],"読んだ","読んでいるところだ"))
  ,("伝える",("tell",[PaO,CT],[P 0,Ni,P 1,Wo],"伝えた","伝えているところだ"))
  ,("持っている",("have",[Th],[P 0,Wo],"持っていた","持とうとしている"))
  ,("去る",("leave",[Pl],[P 0,Wo],"去った","去ろうとしている"))
  ]

nounC :: M.Map Noun Athe 
nounC = M.fromList
  [("apple","an"),("cake","a"),("pencil","a"),("note","a"),("chair","a")
  ,("picture","a"),("book","a"),("message","a")
  ,("school",""),("library","the"),("town","the")
  ,("Tom",""),("Mary",""),("Kenta","")
  ,("hello",""),("thanks","")
  ,("him",""),("her",""),("you","")]

nounT :: M.Map Noun Wtype
nounT = M.fromList
  [("apple",Fo),("cake",Fo),("pencil",Th),("note",Th),("chair",CS)
  ,("picture",CD),("book",CR),("message",CT)
  ,("school",Pl),("library",Pl),("town",Pl)
  ,("Tom",Pa),("Mary",Pa),("Kenta",Pa)
  ,("hello",Gr),("thanks",Gr)
  ,("him",PaO),("her",PaO),("you",PaO)]

nounJ :: M.Map Noun JpNoun 
nounJ = M.fromList
  [("apple","りんご"),("cake","ケーキ"),("pencil","鉛筆"),("note","ノート"),("chair","椅子")
  ,("picture","絵"),("book","本"),("message","メッセージ")
  ,("school","学校"),("library","図書館"),("town","その街")
  ,("Tom","トム"),("Mary","マリー"),("Kenta","健太")
  ,("hello","こんにちは"),("thanks","ありがとう")
  ,("him","彼"),("her","彼女"),("you","あなた")]


getRand :: Int -> IO Int
getRand i = randomRIO (0,i-1)

makeVerbChange :: Int -> M.Map JpVerb (Verb,[Wtype],[Jtype],JpVerb,JpVerb) -> IO (T.Text,T.Text)
makeVerbChange 0 _ = return ("","")
makeVerbChange i verbL = do
  let verSize = M.size verbL
  vr <- getRand verSize
  let (jverb,(everb,_,_,_,_)) = M.elemAt vr verbL
  let nverbL = M.deleteAt vr verbL
  let nverbL' = if nverbL==M.empty then verbJ else nverbL
  let everbP = makeVerb Am False False everb
  wv <- getRand 2
  let qverbN = if wv==0 then "<     >" else everb
  let qverbP = if wv==1 then "<     >" else everbP
  (newQuestion,newAnswer) <- makeVerbChange (i-1) nverbL'
  let question = T.pack (show i) <> ".  " <> jverb <> ":   現在形: " <> qverbN <> "   過去形: " <> qverbP 
  let answer = T.pack (show i) <> ".  " <> jverb <> ":   現在形: " <> everb <> "   過去形: " <> everbP 
  return (newQuestion <> question <> "\n", newAnswer <> answer <> "\n")


makeSentence :: Int -> Qtype -> M.Map JpSubject Subject -> M.Map JpVerb (Verb,[Wtype],[Jtype],JpVerb,JpVerb)
                    -> IO (T.Text,T.Text) 
makeSentence 0 _ _ _ = return ("","")
makeSentence i qt@(isub,iverN,iverP,iverI) sujL verbL = do
  let subSize = M.size sujL  
  sr <- getRand subSize
  let (jsub,esub) = M.elemAt sr sujL
  let nsujL = M.deleteAt sr sujL
  let be = fromMaybe Be (M.lookup esub subB)
  let verSize = M.size verbL
  vr <- getRand verSize
  let (jverb,(everb,verbteL,verbtjL,jverbP,jverbI)) = M.elemAt vr verbL
  let nverbL = M.deleteAt vr verbL
  wr <- getRand 2
  let ipr = if iverN && iverP then wr==0 else iverN || not (iverP || False) 
  let verb = makeVerb be ipr iverI everb
  tseL <- mapM typeToString verbteL
  let tsjL = map (jtypeToString tseL) verbtjL
  let eresL = esub : (if iverI then beVerb be else "") : verb : tseL 
  let jresL = qWord S True qt jsub : tsjL 
                          ++ [qWord V True qt (if iverI then jverbI else if ipr then jverb else jverbP)] 
  let qresL = qWord S False qt esub : qWord V False qt verb : tseL
  let nsujL' = if nsujL==M.empty then subJ else nsujL 
  let nverbL' = if nverbL==M.empty then verbJ else nverbL
  (newQuestion,newAnswer) <- makeSentence (i-1) qt nsujL' nverbL'
  let question = (T.pack (show i) <> ".  " <> T.unwords jresL) <> "\n" <> ("  " <> T.unwords qresL <> ".")
  let answer = T.pack (show i) <> ".  " <> T.unwords eresL <> "." 
  return (newQuestion <> question <> "\n", newAnswer <> answer <> "\n")

beVerb :: Be -> T.Text 
beVerb be = case be of Am -> "am "; Are -> "are "; Is -> "is "; _ -> "be "

makeVerb :: Be -> Bool -> Bool -> Verb -> Verb
makeVerb be b b2 ev = 
  let verb 
        | b2 = M.lookup ev verbIng
        | b && be==Is = M.lookup ev verbNow
        | b = Just ev
        | otherwise = M.lookup ev verbPast 
   in fromMaybe "" verb 

qWord :: WClass -> Bool -> Qtype -> T.Text -> T.Text 
qWord wc b (isub,iverN,iverP,iverI) wd
   |b = if wc==S && isub then putChars '[' ']' wd 
                         else if wc==V && (iverN || iverP || iverI) then putChars '<' '>' wd else wd
   |wc==S && isub = "[     ]" 
   |wc==V && (iverN || iverP) = "<     >"
   |wc==V && iverI = "<             >"
   |otherwise = wd

putChars :: Char -> Char -> T.Text -> T.Text 
putChars h l str = T.pack $ h:T.unpack str++[l]

jtypeToString :: [T.Text] -> Jtype -> T.Text 
jtypeToString tL jt =
  case jt of
    Wo -> "を"
    Ni -> "に"
    Kr -> "から"
    T -> "と"
    P i -> let wd = tL!!i in fromMaybe "" (M.lookup (last (T.words wd)) nounJ)

typeToString :: Wtype -> IO T.Text 
typeToString wt =
  case wt of
    To -> return "to"
    Fr -> return "from"
    On -> return "on"
    Th -> typeToNoun [Th,Fo,CS] 
    wt' -> typeToNoun [wt'] 

typeToNoun :: [Wtype] -> IO T.Text 
typeToNoun ts = do
    let thsL = M.filter (`elem` ts) nounT
        thsSize = M.size thsL
    tr <- getRand thsSize
    let noun = fst$M.elemAt tr thsL 
        Just athe = M.lookup noun nounC
        res = if athe=="" then noun else athe<>" "<>noun
    return res 

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

strQToLatex :: Bool -> T.Text -> T.Text 
strQToLatex b str =
  let lns = T.lines str
      nlns = map cnvSpace lns
      res = if b then listQ2ToLatex nlns else listQToLatex nlns
   in T.unlines res

cnvSpace :: T.Text -> T.Text 
cnvSpace str = 
  let str2 = if  "     " `T.isInfixOf` str then T.replace "     " "\\hspace{3em}" str else str
      str3 = if  "   " `T.isInfixOf` str2 then T.replace "   " "\\hspace{2em}" str2 else str2
   in str3 

listQToLatex :: [T.Text] -> [T.Text]
listQToLatex [] = []
listQToLatex [x] = []
listQToLatex (x:y:xs) = x:"":y:"\\\\":"":listQToLatex xs

listQ2ToLatex :: [T.Text] -> [T.Text]
listQ2ToLatex [] = []
listQ2ToLatex (x:xs) = x:"\\\\":"":listQ2ToLatex xs

listAToLatex :: [T.Text] -> [T.Text]
listAToLatex [] = []
listAToLatex (x:xs) = x:"":listAToLatex xs

makePages :: [String] -> (T.Text,T.Text) -> IO (T.Text,T.Text)
makePages arg (pq,pa) = do
  let nl = if null arg then 15 else read (head arg) :: Int
      qts = if null (tail arg) then "3" else head$tail arg
      qt = if all isDigit qts then read qts :: Int else 0
      isub = qt==1 || qt==3 || qt==5 || qt==7 || qt==9
      iverN = qt==2 || qt==3 || qt==6 || qt==7
      iverP = qt==4 || qt==5 || qt==6 || qt==7
      iverI = qt==8 || qt==9
      ioVerP = qt==0 && qts=="vp"
  (q,a) <- if ioVerP then makeVerbChange nl verbJ else makeSentence nl (isub,iverN,iverP,iverI) subJ verbJ
  let hd0 = if isub then "\\scriptsize 主語ー" else ""
      hd1 = if iverN then hd0<>"\\scriptsize 動詞現在形ー" else hd0
      hd2 = if iverP then hd1<>"\\scriptsize 動詞過去形ー" else hd1
      hd3 = if iverI then hd2<>"\\scriptsize 動詞現在進行形ー" else hd2
      hd = if ioVerP then "\n\\lhead{\\scriptsize 動詞ー現在・過去 活用練習}\n" else "\n\\lhead{"<>hd3<>"英文練習}\n"
      nq = pq <> hd <> strQToLatex ioVerP q
      na = pa <> hd <> T.unlines (listAToLatex (T.lines a))
  if length arg > 2 then makePages (drop 2 arg) (nq<>"\n\\newpage",na<>"\n\\newpage")
                    else return (nq,na)

  {--
makeParts :: IO ()
makeParts = do
  vs <- fileReadT "verb.txt"
  let vlns = T.lines vs
  let vpart = makeVerbPart vlns
  let (jv,vc) = unzip vpart
  let (vs,vp) = unzip vc
  let jvm = M.fromList jv
  let vsm = M.fromList vs
  let vpm = M.fromList vp
  print jvm
  print vsm
  print vpm
  return ()

makeVerbPart :: [T.Text] -> [((JpVerb,(Verb,[Wtype],[Jtype],JpVerb,JpVerb)),((Verb,Verb),(Verb,Verb),(Verb,Verb)))] 
makeVerbPart [] = []
makeVerbPart (v:vs) =
  let (everb:everbN:everbP:everbF:everbI:jverb:jverbP:jverbI:verbteL:verbtjL:xs) = T.splitOn " " v
   in ((jverb,(everb,verbteL,verbtjL,jverbP,jverbI)),((everb,everbN),(everb,everbP),(everb,everbI))):makeVerbPart vs
--}

main :: IO ()
main = do
  arg <- getArgs
  (q,a) <- makePages arg ("","")
  let lq = latexHeader <> "\\begin{document}\n" <> q <> "\\end{document}" 
      la = latexHeader <> "\\begin{document}\n" <> a <> "\\end{document}"
  fileWriteT "eigoQ.tex" lq
  fileWriteT "eigoA.tex" la
  return ()

