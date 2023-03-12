module Siki (siki) where

import Define (Def)
import Libs(getIndex,isNum)

wake :: String -> ([[Char]],[Char])
wake [] = ([[]],[])
wake (x:xs)
  | x `elem` "0123456789" = ((x:hn):ns,cs)
  | x `elem` "+-*^" = ([]:hn:ns,x:cs)
  | otherwise = ((x:hn):ns,cs) 
  where ((hn:ns),cs) = wake xs

makeNum :: [Def] -> [[Char]] -> [Integer]
makeNum _ [] = []
makeNum def (x:xs) =
  let (cha,num) = unzip def
   in if(x/=[]&&isNum x) then read x:(makeNum def xs)
                         else if(elem x cha)
                    then (num!!getIndex x cha):(makeNum def xs) else []

evl :: (Integer->Integer->Integer)->Char->[Char]->[Integer]->([Char],[Integer])
evl f op ops nms =
  let id=getIndex op ops
      rs=if(op=='^') then f (nms!!(id+1)) (nms!!id) else f (nms!!id) (nms!!(id+1))
      ops'=(take id ops)++(drop (id+1) ops)
      nms'=(take id nms)++[rs]++(drop (id+2) nms)
   in if (op=='^') then (reverse ops',reverse nms') else (ops',nms')

evaluate :: [Char] -> [Integer] -> [Integer]
evaluate _ [] = []
evaluate ops nms
  | ((length ops+1)/=(length nms)) = [] 
  | elem '^' ops = let (ops',nms')= evl (^) '^' (reverse ops) (reverse nms) in evaluate ops' nms' 
  | elem '*' ops = let (ops',nms')=evl (*) '*' ops nms in evaluate ops' nms'
  | elem '+' ops = let (ops',nms')=evl (+) '+' ops nms in evaluate ops' nms'
  | elem '-' ops = let (ops',nms')=evl (-) '-' ops nms in evaluate ops' nms'
  | otherwise = nms 

siki :: [Def] -> String -> String
siki def str = let (nms,ops) = wake str
                   res = evaluate ops (makeNum def nms)
                in if(res==[]) then str else show$last res

