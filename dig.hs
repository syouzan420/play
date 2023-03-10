import System.Random

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

data Dir = LF | RT | DW | ER deriving (Eq,Show)

wat :: Pos -> String -> IO ()
wat p xs = do goto p
              putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC["++show y++";"++show x++"H")

width :: Int
width = 5 

height :: Int
height = 5

migi :: Int
migi = 5

getRand :: Int -> IO Int
getRand i = randomRIO (0,i)

nextDir :: [Pos] -> [Dir] 
nextDir ((x,y):ps)
  | x==0 = if((x+1,y) `elem` ps) then [DW] else [RT,DW]
  | x==(width-1) = if((x-1,y) `elem` ps) then [DW] else [LF,DW]
  | (x+1,y) `elem` ps = [LF,DW]
  | (x-1,y) `elem` ps = [RT,DW]
  | otherwise = [LF,RT,DW]

nextPos :: [Pos] -> IO Pos
nextPos ps = do
  let rp = reverse ps
      (x,y) = head rp
      nds = nextDir rp
  n <- getRand (length nds-1)
  let nd = nds!!n
      (nx,ny) = case nd of
                  LF -> (x-1,y)
                  RT -> (x+1,y)
                  DW -> (x,y+1)
  return (nx,ny)

makePos :: [Pos] -> IO [Pos]
makePos [] = let center = if(width `mod` 2==0) then (div width 2)-1 
                                               else div (width-1) 2 
              in makePos [(center,0)]
makePos ps = do
  (nx,ny) <- nextPos ps
  if (ny==height) then return ps
                  else makePos (ps++[(nx,ny)])

sepPos :: [Pos] -> [[Int]]
sepPos = spPos 0

--spPos :: Int -> [Pos] -> [[Int]]
--spPos _ [] = []
--spPos i ((x,y):ps) 
--  | null ps = if(i==y) then [[x]] else []:[[x]]
--  | otherwise = if(i==y) then (x:(head$spPos i ps)):(tail$spPos i ps)
--                         else []:(x:(head$spPos (i+1) ps)):(tail$spPos (i+1) ps) 

spPos :: Int -> [Pos] -> [[Int]]
spPos _ [] = []
spPos i ((x,y):ps) 
  | null ps = if(i==y) then [[x]] else []:[[x]]
  | otherwise = if(i==y) then addList i else []:(addList (i+1))
  where addList n = (x:(head$spPos n ps)):(tail$spPos n ps)


test :: [Pos]
test = [(3,0),(2,0),(2,1),(3,1),(4,1),(4,2),(4,3),(3,3),(3,4),(2,4)]
  
makeRow :: [Int] -> String
makeRow = mkRow 0

mkRow :: Int -> [Int] -> String
mkRow n is
  |n==width = []
  |otherwise = if (n `elem` is) then 'o':mkRow (n+1) is 
                                else 'x':mkRow (n+1) is

showNums :: [String] -> [String]
showNums [row] = [concat$map show (rowNumb row)]
showNums (r:s:rs) = [concat$map show$ 
  map (\(n,d) -> n+d) (zip (rowNumb r) (rowNumbWith s))]
    ++(showNums (s:rs))

rowNumbWith :: String -> [Int]
rowNumbWith row =
  map (\(n,x) -> if(x=='o') then n+1 else n) (zip (rowNumb row) row)

rowNumb :: String -> [Int]
rowNumb = rowNum 'x'

rowNum :: Char -> String -> [Int]
rowNum ch [x] = if(ch=='o') then [1] else [0]
rowNum ch (a:b:tl)
  | ch=='o' = if(b=='o') then 2:rn else 1:rn
  | otherwise = if(b=='o') then 1:rn else 0:rn
  where rn = rowNum a (b:tl)

makeG :: Int -> Pos -> [String] -> [String] 
makeG _ _ [] = []
makeG i (x,y) (r:rs) =
  let nrow = if(i==y) then (take (x-1) r ++ "@" ++ drop x r) else r
   in nrow:(makeG (i+1) (x,y) rs)

showPlayer :: Pos -> IO ()
showPlayer (x,y) = do
  goto (x,y+1) 
  putStr "@"
  goto (0,8)

main :: IO ()
main = do
  ps <- makePos []
  let sp = sepPos ps
      rws = map makeRow sp
      nmb = showNums rws
      px = if(mod width 2==0) then div width 2 else div width 2 + 1
  cls
  goto (px+migi,0)
  putStrLn "@"
  mapM putRowLn nmb 
  getLine
  gameLoop ((px,1),rws)
  return ()

gameLoop :: (Pos,[String]) -> IO ()
gameLoop ((x,y),rws) = do
  cls
  goto (0,2)
  mapM putRowLn (showNums rws) 
  showPlayer (x+migi,y)
  putStr "   Dig: j, GoLeft: h, GoRight: l -> "
  c <- getLine
  if(c/="" && head c `elem` "jhl") 
     then movePlayer (x,y) (updatePos (head c) (x,y)) rws 
     else gameLoop ((x,y),rws)

updatePos :: Char -> Pos -> Pos
updatePos ch (x,y) = case ch of
                       'j' -> (x,y+1)
                       'h' -> (x-1,y)
                       'l' -> (x+1,y)

movePlayer :: Pos -> Pos -> [String] -> IO ()
movePlayer ps (nx,ny) rws = do
  if(nx<0 || nx>width) 
     then putStrLn "    Can't move!" >> gameLoop (ps,rws) 
     else if(ny>height) 
            then gameClear
            else if(checkBlock (nx,ny) rws)
                    then gameLoop ((nx,ny),rws) 
                    else gameOver ps rws

checkBlock :: Pos -> [String] -> Bool
checkBlock (x,y) rws = let ch = (rws!!(y-1))!!(x-1)
                        in if(ch=='o') then True else False

gameClear :: IO ()
gameClear = putStrLn "    Congratrations!"

gameOver :: Pos -> [String] -> IO ()
gameOver (x,y) rws = do
  cls
  goto (0,2)
  mapM putRowLn rws 
  showPlayer (x+migi,y) 
  putStrLn "    Game Over"

putRowLn :: String -> IO ()
putRowLn str = putStrLn (replicate migi ' ' ++ str)
