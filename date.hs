import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))

day :: IO String 
day = do
  da <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return ((take 4 da) ++ (take 2 (drop 5 da)) ++ (drop 8 da))

wake :: String -> (Int, Int, Int)
wake a = (read (take 4 a), read (take 2 (drop 4 a)), read (drop 6 a))

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

uru :: Int -> Bool
uru y = let r1 = mod y 4 == 0
            r2 = mod y 100 == 0
            r3 = mod y 400 == 0
         in r3 || (r1 && not r2)

nen :: String -> Int
nen a = let (y,m,d) = wake a
            iuru = uru y
         in if(m==1) then d
                     else sum (take (m-1) daylist) + d 
                          + (if (m>2 && iuru) then 1 else 0)

daynow :: String -> IO Int
daynow a = do
  nday <- day
  let (y,m,d) = wake a
      (yn,_,_) = wake nday
      fyday = 365 - nen a + (if (uru y) then 1 else 0)
      lsday = nen nday 
  return (fyday + mdday (y+1) (yn-1) + lsday)

mdday :: Int -> Int -> Int
mdday c t
  | c==t = if (uru t) then 366 else 365
  | otherwise = (if(uru c) then 366 else 365) + (mdday (c+1) t)
