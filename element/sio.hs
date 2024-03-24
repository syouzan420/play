{-
a % の食塩水 b g の食塩の量 c g
塩を入れる
水を入れる
蒸発させる
食塩水を入れる

最初の食塩水
1. 濃度a%の食塩水bグラム
2. 食塩aグラムをとかした濃度b%の食塩水
3. a グラムの水に食塩をとかした b%の食塩水
4. 食塩aグラムをとかしたbグラムの食塩水
5. 食塩aグラムに水をbグラムとかした食塩水
-}

import System.Random (randomRIO)
import Data.Ratio

type Nou = Int
type Sio = Int
type Miz = Int
type Zen = Int
data Sol = NZ Nou Zen | NS Nou Sio | NM Nou Miz 
         | SZ Sio Zen | SM Sio Miz deriving (Eq,Show)

getInt :: Int -> Int -> IO Int
getInt i j = randomRIO (i,j) 

showR :: Ratio Int -> String
showR r = let nu = numerator r
              de = denominator r
           in if de==1 then show nu else 
                    let sh = bTos r
                        isRoop = '(' `elem` sh
                     in if isRoop then show nu<>"/"<>show de else sh 

bTos :: Ratio Int -> String
bTos r = 
  let nu = numerator r
      de = denominator r
      di = nu `div` de
      re = nu `rem` de
   in if re==0 then show di else show di<>"."<>bTos' [re] (re*10) de

bTos' :: [Int] -> Int -> Int -> String
bTos' rms n d =
  let di = n `div` d
      re = n `rem` d
      isInRms = re `elem` rms
      roopNum = if isInRms then 1 + getIndex re rms else 0
      eStr = if isInRms then " ("<>show roopNum<>")" else ""
   in if re==0 || isInRms then show di<>eStr else show di<>bTos' (re:rms) (re*10) d 

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if t==x then 0 else 1+getIndex t xs

main:: IO ()
main = do
  zen <- getInt 10 80 >>= return.(*10) 
  nou <- getInt 3 15
  let sio = (zen % 1) * (nou % 100)  
  let miz = (zen % 1) - sio 
  solNum <- getInt 0 4
  let solStr = case solNum of
        0 -> "濃度 "<>show nou<>" % の食塩水 "<>show zen<>" g "
        1 -> "食塩が "<>showR sio<>" g とけている 濃度 "<>show nou<>" % の食塩水"
        2 -> showR miz<>" g の水に 食塩をとかした 濃度 "<>show nou<>" % の食塩水"
        3 -> "食塩を "<>showR sio<>" g とかした "<>show zen<>" g の食塩水"
        4 -> showR miz<>" g の水に "<>showR sio<>" g の食塩をとかした食塩水"
        _ -> ""
  putStrLn solStr 
