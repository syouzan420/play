import System.Random (randomRIO)
import qualified Data.Map.Strict as M
import Myfile

data Be = Am | Are | Is
data Vtype = V | C | O | OC | OO
data Ztype = Wo | Ni
data Ntype = Th | Fo | Pl -- Thing, Food, Place
data Nc = Cn | Un -- Countable, Uncountable

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

verbNow :: M.Map Verb VerbS 
verbNow = M.fromList 
  [("give","gives"),("take","takes"),("buy","buys"),("eat","eats"),("see","sees")
  ,("go","goes"),("come","comes")]

verbJ :: M.Map JpVerb Verb 
verbJ = M.fromList
  [("あげる","give"),("とる","take"),("買う","buy"),("食べる","eat"),("見る","see")
  ,("行く","go"),("来る","come")]

nounC :: M.Map Noun Nc
nounC = M.fromList
  [("apple",Cn),("cake",Cn),("pencil",Cn)]

nounT :: M.Map Noun Ntype
nounT = M.fromList
  [("apple",Fo),("cake",Fo),("pencil",Th)]

nounJ :: M.Map JpNoun Noun 
nounJ = M.fromList
  [("りんご","apple"),("ケーキ","cake"),("鉛筆","pencil")]

getRand :: Int -> IO Int
getRand i = randomRIO (0,i)

makeSentence :: M.Map JpSubject Subject -> IO [String]
makeSentence sujL = do
  let subSize = M.size sujL  
  sr <- getRand (subSize - 1)
  let (jsub,esub) = M.elemAt sr sujL
      nsuj = M.deleteAt sr sujL
  putStrLn jsub
  return [jsub]


