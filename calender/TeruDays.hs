module TeruDays (today,howLong,howManyDays,isDay,daysFromBirth,daysFromTheYear,daysBetweenYears) where

import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))

type YearMonthDay = String
type HourMinute = String
type Year = Int; type Month = Int; type Day = Int; type Hour = Int; type Minute = Int
data DayType = Y | M | W | D | H deriving (Eq, Show) 

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

weeklist :: [String]
weeklist = ["su","m","tu","w","th","f","sa"]

today :: IO YearMonthDay 
today = do
  (a:b:c:d:_:e:f:_:g) <- (show <$> localDay) . zonedTimeToLocalTime <$> getZonedTime
  return (a:b:c:d:e:f:g)

howLong :: HourMinute -> HourMinute -> Minute 
howLong s f =
  let (sho,smi) = splitHourMinute s
      (fho,fmi) = splitHourMinute f
      sami = sho * 60 + smi
      fami = fho * 60 + fmi
   in fami - sami

howManyDays :: YearMonthDay -> YearMonthDay -> Day 
howManyDays sday fday = do
  let (y,m,d) = splitYearMonthDay sday 
      (yn,_,_) = splitYearMonthDay fday
      fyday = 365 - daysFromTheYear sday + (if isUru y then 1 else 0)
      lsday = daysFromTheYear fday 
   in (fyday + daysBetweenYears (y+1) (yn-1) + lsday)

isDay :: DayType -> String -> Bool
isDay t s =
  let s' = if head s=='0' then tail s else s 
      len = length s'
   in case t of
        Y -> let (mo,dy) = if len==3 then ([head s'],tail s') else splitAt 2 s'
                 (moi,dyi) = (read mo::Int, read dy::Int)
              in len>2 && len<5 && moi>0 && moi<13 && dyi>0 && dyi<1+(daylist!!(moi-1))
        M -> let dyi = read s'::Int
              in dyi>0 && dyi<32
        W -> s' `elem` weeklist 
        D -> let (mo,dy) = if len==7 then ([s'!!4],drop 5 s') 
                                     else (take 2 (drop 4 s'),drop 6 s')
                 (moi,dyi) = (read mo::Int, read dy::Int)
              in len>6 && len <9 && moi>0 && moi<13 && dyi>0 && dyi<1+(daylist!!(moi-1))
        H -> let (h,m) = if len==3 then ([head s'],tail s') else splitAt 2 s'
                 (hi,mi) = (read h::Int, read m::Int)
              in len>2 && len<5 && hi>=0 && hi<24 && mi>=0 && mi<60

isUru :: Year -> Bool
isUru y = let r1 = mod y 4 == 0
              r2 = mod y 100 == 0
              r3 = mod y 400 == 0
           in r3 || (r1 && not r2)

splitHourMinute :: HourMinute -> (Hour, Minute)
splitHourMinute s =
  let len = length s
      (ho,mi) = if len==3 then ([head s],tail s) else splitAt 2 s 
   in (read ho, read mi)

daysBetweenYears :: Year -> Year -> Day 
daysBetweenYears syear fyear 
  | syear>fyear = daysBetweenYears fyear syear
  | syear==fyear = if isUru fyear then 366 else 365
  | otherwise = (if isUru syear then 366 else 365) + daysBetweenYears (syear+1) fyear 

splitYearMonthDay :: YearMonthDay -> (Year, Month, Day)
splitYearMonthDay (a:b:c:d:e:f:gs) = let (yr,mo,da) = (read [a,b,c,d], read [e,f], read gs)
                         in if mo>12 then (yr,read [e],read (f:gs)) else (yr,mo,da)

daysFromTheYear :: YearMonthDay -> Day 
daysFromTheYear a = let (y,m,d) = splitYearMonthDay a
                     in if m==1 then d
                                else sum (take (m-1) daylist) + d + (if m>2 && isUru y then 1 else 0)

daysFromBirth :: YearMonthDay -> IO Day
daysFromBirth bday = howManyDays bday <$> today
