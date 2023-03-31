import System.Random (randomRIO)
import qualified Data.Map.Strict as M
import Myfile

data Be = Am | Are | Is
data Vtype = V | C | O | OC | OO
data Ztype = Wo | Ni
data Ntype = Th | Fo | Pl -- Thing, Food, Place
data Nc = C | U -- Countable, Uncountable

subB :: M.Map String Be
subB = M.fromList 
  [("I",Am),("You",Are),("We",Are),("He",Is),("She",Is),("They",Are),("It",Is)]

subJ :: M.Map String String
subJ = M.fromList 
  [("私は","I"),("あなたは","You"),("あなたたちは","You"),("私たちは","We")
  ,("彼は","He"),("彼女は","She"),("彼らは","They"),("それらは","They"),("それは","It")]

verbNow :: M.Map String String
verbNow = M.fromList 
  [("give","gives"),("take","takes"),("buy","buys"),("eat","eats"),("see","sees")
  ,("go","goes"),("come","comes")]

verbJ :: M.Map String String
verbJ = M.fromList
  [("あげる","give"),("とる","take"),("買う","buy"),("食べる","eat"),("見る","see")
  ,("行く","go"),("来る","come")]

nounC :: M.Map String Nc
nounC = M.fromList
  [("apple",C),("cake",C),("pencil",C)]

nounT :: M.Map String NType
nounT = M.fromList
  [("apple",Fo),("cake",Fo),("pencil",Th)]

nounJ :: M.Map String String
nounJ = M.fromList
  [("りんご","apple"),("ケーキ","cake"),("鉛筆","pencil")]

getRand :: Int -> IO Int
getRand i = randomRIO (0,i)

