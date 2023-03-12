
import System.IO.HiddenChar
import System.IO (hSetBuffering,hFlush,stdout,BufferMode(NoBuffering))

type Pos = (Int,Int)
type Mana = Char 
type Cell = (Mana,Mode)
type Grid = [[Cell]]
type Stage = [(Pos,Cell)]
type Size = (Int,Int)
type Fpon = (Char,Func)
type Spon = (Int,Func2)
type Func = (Char->Char)
type Func2 = (Char->Char->Char)
type Def = (String,Integer)
type Msg = [String]
type Evt = (String,String)    --(Triger Event, Target Event)

data Mode = Fr | Bl | Mv | Pn | DB | DF deriving (Eq,Show)

data Dir = Up | Dw | Lf | Rt | Cn deriving (Eq,Show)

ix, iy :: Int
ix = 5; iy = 3

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC["++show y++";"++show x++"H")

hideCur :: IO ()
hideCur = putStr "\ESC[?25l"

showCur :: IO ()
showCur = putStr "\ESC[?25h"

chColor1 :: IO ()
chColor1 = putStr "\ESC[36m"

chColor2 :: IO ()
chColor2 = putStr "\ESC[31m"

chColor3 :: IO ()
chColor3 = putStr "\ESC[33m"

chColor4 :: IO ()
chColor4 = putStr "\ESC[34m"

chColor5 :: IO ()
chColor5 = putStr "\ESC[35m"

mkDefault :: IO ()
mkDefault = putStr "\ESC[39m"

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if(t==x) then 0 else 1+(getIndex t xs)

keyCheck :: Size -> Pos -> Char -> Pos 
keyCheck (wd,hi) (x,y) ch  
  | ch=='j'||ch=='P' = if(hi==y+1) then (x,y) else (x,y+1)
  | ch=='k'||ch=='H' = if(0>y-1) then (x,y) else (x,y-1)
  | ch=='h'||ch=='K' = if(0>x-1) then (x,y) else (x-1,y)
  | ch=='l'||ch=='M' = if(wd==x+1) then (x,y) else (x+1,y)
  | otherwise = (x,y) 

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

putGrid :: Grid -> IO ()
putGrid grid = do 
  cls
  goto (ix,iy)
  putStrLn edgeLine
  putInside grid
    where gridWidth = length (head grid)
          edgeLine = ("-"++(replicate gridWidth '-')++"-")
          leftSpace = replicate (ix-1) ' '
          putInside [] = putStrLn (leftSpace++edgeLine)
          putInside (g:gs) = do
            putStr (leftSpace++"|")
            mapM_ (\(ch,tp)->(case tp of
                               Bl -> chColor3;Mv -> chColor2;Pn -> chColor1;
                               DB -> chColor4;DF -> chColor5;
                               _  -> return ())>>putStr [ch]>>mkDefault) g
            putStrLn "|"
            putInside gs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter Stage Size x: "
  sx <- getLine
  putStr "Enter Stage Size y: "
  sy <- getLine
  let sz = (read sx::Int,read sy::Int)
  hideCur
  cls
  let igrid = makeGrid sz []
  putGrid igrid 
  goto (ix+1,iy+1)
  putStr "@"
  mapLoop (0,0) sz False igrid []
  showCur

mapLoop :: Pos -> Size -> Bool -> Grid -> Stage -> IO ()
mapLoop (x,y) sz md grid stage = do
  i <- getHiddenChar
  let (wd,hi) = sz
      (x',y') = if md then (x,y) else keyCheck (wd,hi) (x,y) i
      nmd = if (i=='i') then not md else md 
  (grid',stage') <- if nmd then do
        putStr ("Type Character:")
        c <- getHiddenChar
        putStr ([c]++
            "\n Type?(b:block,f:free,m:move,p:pon,d:darkblock,e:darkfree):")
        t <- getHiddenChar 
        let tp = case t of 'b'->Bl;'f'->Fr;'m'->Mv;'p'->Pn;
                           'd'->DB;'e'->DF;_->Fr
            ngrid = let gline = grid!!y
                        ngline = take x gline ++ [(c,tp)] ++ drop (x+1) gline 
                     in take y grid ++ [ngline] ++ drop (y+1) grid
            (xys,_) = unzip stage
            icon = elem (x',y') xys 
            id = if icon then getIndex (x',y') xys else (-1)
            pstage = if icon then take id stage ++ drop (id+1) stage else stage
            nstage = pstage ++ [((x',y'),(c,tp))]
        return (ngrid,nstage)
                 else return (grid,stage)
  putGrid grid' 
  goto (x'+ix+1,y'+iy+1)
  if nmd then return () else putStr "@"
  if i=='\ESC' then do
                  goto (0,iy+hi+3)
                  print stage'
               else mapLoop (x',y') sz False grid' stage'

