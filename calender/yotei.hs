module Main where

import TeruDays (Year,Month,Day,YearMonthDay,weeklist,today,howManyDays,isUru,daylist)

yobiBase :: YearMonthDay 
yobiBase = "20200419" 

yobi :: YearMonthDay -> Int
yobi day = let days = howManyDays yobiBase day 
               yobi = days `mod` 7
            in if day>=yobiBase then yobi else 7 - yobi

dayList :: Year -> Month -> [(Day,Int)]
dayList ye mo =
  let firstDay = show ye ++ show mo ++ "01"
      firstDaysYobi = yobi firstDay
      iu = isUru ye
      lastDay = if mo==2 && iu then 29 else daylist!!(mo-1)
   in zip [1..lastDay] ([firstDaysYobi..6]++cycle [0::Int,1..6])

spDayList :: [(Day,Int)] -> [[Day]]
spDayList [] = []
spDayList [(x,_)] = [[x]]
spDayList ((dy,wd):xs) 
  |wd==6 = [dy]:spDayList xs
  |otherwise = (dy:h) : t 
  where (h:t) = spDayList xs
  
fillDayList :: [[Day]] -> [[Day]]
fillDayList dlst =
  let fweek = head dlst
      fwl = length fweek
      lweek = last dlst
      lwl = length lweek
      nfweek = replicate (7-fwl) 0 ++ fweek 
      nlweek = lweek ++ replicate (7-lwl) 0 
   in [nfweek]++(tail.init) dlst++[nlweek]

genMonthHtml :: Year -> Month -> String
genMonthHtml ye mo = title++"<table>"++tbl++"</table>\n"
  where dlst = fillDayList$spDayList$dayList ye mo 
        title = "<a>"++show ye++"年  "++show mo++"月</a>\n"
        hdr = foldl (\acc yb -> acc++"<th>"++yb++"</th>") "" ["日","月","火","水","木","金","土"]
        mdl = foldl (\acc wk -> acc++"<tr>"++
          foldl (\acc2 dy -> acc2++"<td>"++"<a href=\"./\">"++(if dy==0 then "" else show dy)++"</a></td>") "" wk
                                      ++"</tr>") "" dlst
        tbl = hdr++mdl 


main :: IO ()
main = do
  putStrLn "Welcome to Yotei Hyou!"
  -- 空き予定を確認するモードか 空き予定を設定するモードかを選択
  -- 空き予定を確認するモード
  --    カレンダーを確認（または終了）
  --    選んだ日の空き時間を選択
  --    予定を入力
  --    カレンダーの確認にもどる
  -- 空き予定を設定するモード
  --    カレンダーを確認(または終了)
  --    選んだ日の空き時間を入力
  --    カレンダーの確認にもどる

