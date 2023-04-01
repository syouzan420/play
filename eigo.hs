import System.Random (randomRIO)
import qualified Data.Map.Strict as M
import Myfile

data Be = Am | Are | Is deriving Eq
data Wtype = Wo | Ni | Kr | To | Fr | Th | Fo | Pl | Pa  deriving Eq
-- を, に, から, To, From, Thing, Food, Place, Parson
data Nc = Cn | Un deriving Eq -- Countable, Uncountable

type Subject = String
type JpSubject = String
type Verb = String
type VerbS = String
type JpVerb = String
type Noun = String
type JpNoun = String

subB :: M.Map Subject Be
subB = M.fromList 
  [("I",Am),("You",Are),("We",Are),("He",Is),("She",Is),("They",Are),("It",Is)]

subJ :: M.Map JpSubject Subject 
subJ = M.fromList 
  [("私は","I"),("あなたは","You"),("あなたたちは","You"),("私たちは","We")
  ,("彼は","He"),("彼女は","She"),("彼らは","They"),("それらは","They"),("それは","It")]

verbTj :: M.Map JpVerb [Wtype] 
verbTj = M.fromList
  [("あげる",[Th,Wo,Pa,Ni]),("もらう",[Pa,Kr,Th,Wo]),("つれていく",[Pa,Wo,Pl,Ni]),("買う",[Th,Wo])
  ,("食べる",[Fo,Wo]),("見る",[Th,Wo]),("行く",[Pl,Ni]),("来る",[Pl,Ni])]

verbTe :: M.Map Verb [Wtype] 
verbTe = M.fromList
  [("give",[Th,To,Pa]),("take",[Pa,To,Pl]),("buy",[Th]),("eat",[Fo]),("see",[Th])
  ,("go",[To,Pl]),("come",[To,Pl])]

verbNow :: M.Map Verb VerbS 
verbNow = M.fromList 
  [("give","gives"),("take","takes"),("buy","buys"),("eat","eats"),("see","sees")
  ,("go","goes"),("come","comes")]

verbJ :: M.Map JpVerb Verb 
verbJ = M.fromList
  [("あげる","give"),("もらう","take"),("つれていく","take"),("買う","buy"),("食べる","eat")
  ,("見る","see"),("行く","go"),("来る","come")]

nounC :: M.Map Noun Nc
nounC = M.fromList
  [("apple",Cn),("cake",Cn),("pencil",Cn),("note",Cn),("school",Un),("library",Cn)]

nounT :: M.Map Noun Wtype
nounT = M.fromList
  [("apple",Fo),("cake",Fo),("pencil",Th),("note",Th),("school",Pl),("library",Pl)]

nounJ :: M.Map JpNoun Noun 
nounJ = M.fromList
  [("りんご","apple"),("ケーキ","cake"),("鉛筆","pencil"),("ノート","note")
  ,("学校","school"),("図書館","library")]

getRand :: Int -> IO Int
getRand i = randomRIO (0,i)

makeSentence :: M.Map JpSubject Subject -> M.Map JpVerb Verb -> IO [String]
makeSentence sujL verbL = do
  let subSize = M.size sujL  
  sr <- getRand (subSize - 1)
  let (jsub,esub) = M.elemAt sr sujL
      nsujL = M.deleteAt sr sujL
      Just be = M.lookup esub subB
      verSize = M.size verbJ
  vr <- getRand (verSize - 1)
  let (jverb,everb) = M.elemAt vr verbL
      nverbL = M.deleteAt vr verbL
      Just verbN = if be==Is then M.lookup everb verbNow else Just everb
      Just verbteL = M.lookup everb verbTe
      Just verbTjL = M.lookup jverb verbTj
  putStrLn jsub
  return [jsub]

typeToString :: WType -> IO String
typeToString wt =
  case wt of
    Wo -> undefined -- を
    Ni -> undefined -- に
    Kr -> undefined -- から
    To -> undefined -- to
    Fr -> undefined -- from
    Th -> undefined -- Thing
    Fo -> undefined -- Food
    Pl -> undefined -- Place
    Pa -> undefined -- Parson
    _  -> return ""


