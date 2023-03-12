module Grid (intoGrid,fromGrid,makeGrid,toSee,toHide) where

import Define (Pos,Cell,Stage,Size,Grid,Mode(..))

intoGrid :: Pos -> Cell -> Grid -> Grid
intoGrid (x,y) (ch,tp) grid =
  let gline = grid!!y
      ngline = take x gline ++ [(ch,tp)] ++ drop (x+1) gline
   in take y grid ++ [ngline] ++ drop (y+1) grid

fromGrid :: Pos -> Grid -> Cell
fromGrid (x,y) grid = (grid!!y)!!x

makeGrid :: Size -> Stage -> Grid
makeGrid (wd,hi) sta =
  flatToGrid wd $
  map (\(x,y) -> findxy (x,y) sta) [(x,y)|y<-[0..(hi-1)],x<-[0..(wd-1)]]
    where findxy _ [] = (' ',Fr) 
          findxy (a,b) (((p,q),scell):xs) =
                  if(a==p&&b==q) then scell else findxy (a,b) xs 

flatToGrid :: Int -> [a] -> [[a]]
flatToGrid _ [] = []
flatToGrid wd ls = [take wd ls]++(flatToGrid wd (drop wd ls))


toSee :: Grid -> Grid
toSee [] = []
toSee (l:ls) = toSeeLine l:(toSee ls)
  where toSeeLine [] = []
        toSeeLine ((ch,tp):xs)
          |ch/=' ' && tp==DF = (ch,Fr):(toSeeLine xs)
          |ch/=' ' && tp==DB = (ch,Bl):(toSeeLine xs)
          |otherwise = (ch,tp):(toSeeLine xs)

toHide :: Grid -> Grid
toHide [] = []
toHide (l:ls) = toHideLine l:(toHide ls)
  where toHideLine [] = []
        toHideLine ((ch,tp):xs)
          |ch/=' ' && tp==Fr = (ch,DF):(toHideLine xs)
          |ch/=' ' && tp==Bl = (ch,DB):(toHideLine xs)
          |otherwise = (ch,tp):(toHideLine xs)

