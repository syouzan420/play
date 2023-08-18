import StrToInt(makeIntList)

data Mn a = Mn a (a->a) 

(.>) :: Mn a -> Mn a -> Mn a
(.>) (Mn t0 y0) (Mn t1 y1) = Mn (y0 t1) y1 

getTai :: Mn a -> a
getTai (Mn t0 _) = t0

intToMn :: Integer -> Mn Integer
intToMn i = Mn i (i +)

main :: IO ()
main = do
  siki <- getLine
  let intList = makeIntList siki
      mnList = map intToMn intList
      result = if null intList then 0 else getTai$foldl (.>) (intToMn 0) mnList 
  print result
  main

