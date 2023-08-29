import Data.Char
import Data.IORef(IORef, newIORef, readIORef, writeIORef) 
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

data Ho = Ho {hf :: [Int] -> (Ho, [Int]), dt :: [Int]}

mkh :: Ho
mkh = Ho f []
  where f x = ((Ho f x), x)

hod :: [Int] -> Ho -> Ho
hod [] h = Ho (hf p) pd  
  where (p, pd) = (hf h) []
hod il h  = Ho g il 
  where g [] = (h, (dt h))
        g x = (hf h) x 

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hd <- newIORef mkh
  loop hd
  return ()

check :: String -> Bool
check a = foldl (\a x -> a && isDigit x) True a 

loop :: IORef Ho -> IO Ho 
loop hd = do
  h <- readIORef hd
  a <- getLine
  case a of
    "q" -> return h 
    "r" -> do
       nh <- rloop h hd 
       writeIORef hd nh
       loop hd
    _   -> do
       if (check a) then do
          let n = read a :: Int
              nh = hod [n] h
          putStrLn (show (dt nh))
          writeIORef hd nh
          loop hd
                    else do
          putStrLn "Insert data | r: previous data, q: quit"
          loop hd

rloop :: Ho -> IORef Ho -> IO Ho 
rloop lh hd = do
  h <- readIORef hd
  let nh = hod [] h
  putStrLn (show (dt nh))
  a <- getLine
  case a of
    "l" -> do
       putStrLn ("last data: "++ (show (dt lh)))
       return lh
    ""  -> do
       writeIORef hd nh
       rloop lh hd 
    _   -> do
       putStrLn "Enter to proceed | l: last data"
       rloop lh hd

-----------
