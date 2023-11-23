module General where

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if t==x then 0 else 1 + getIndex t xs

removeIndex :: Int -> [a] -> [a]
removeIndex i lst = take i lst ++ drop (i+1) lst

toList :: [a] -> Int -> a -> [a]
toList ls i el = take i ls ++ [el] ++ drop (i+1) ls 

toGrid :: [[a]] -> (Int,Int) -> a -> [[a]]
toGrid gr (i,j) el = toList gr j (toList (gr!!j) i el)

