import Data.Tree 
import Data.List (intercalate)

data Elem = El String Int Int

showT :: Tree String -> String
showT (Node x []) = x
showT (Node x tr) = x++", "++showF tr

showF :: Forest String -> String
showF [] = ""
showF tr = "[" ++ intercalate ", " (map showT tr) ++ "]"

addElem :: Elem -> Forest String -> Forest String
addElem (El str 0 0) fo = fo ++ [Node str []]
addElem (El str l r) fo
  | lng > l && l > 0 = let (h,(Node s _):t) = splitAt (lng - l) fo
                           newNode = Node s (t++[Node str []])
                        in h ++ [newNode]
  | r > 0 = let (it,lt) = (init fo,last fo)
                Node x subf = lt  -- last tree
             in if null subf then it ++ [lt, Node str []]
                             else it ++ [Node x (addElem (El str l r) subf)]
  | otherwise = fo ++ [Node str []]
  where lng = length fo

testF = [Node "1" [], Node "2" [], Node "3" [], Node "4" [Node "5" [], Node "6" []], Node "7" []]

