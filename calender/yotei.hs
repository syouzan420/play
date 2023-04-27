module Main where

import TeruDays (YearMonthDay,weeklist,today,howManyDays)

yobiBase :: YearMonthDay 
yobiBase = "20200419" 

yobi :: YearMonthDay -> Int
yobi day = let days = howManyDays yobiBase day 
               yobi = days `mod` 7
            in if day>=yobiBase then yobi else 7 - yobi

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

