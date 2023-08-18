import StrToInt(makeIntList) 


main :: IO ()
main = do
  siki <- getLine 
  let intList = makeIntList siki
      addFs = if null intList then (+) 0 
                              else foldl (\f x -> (+) (f x)) (head intList +) (tail intList) 
      result = addFs 0
  print result
  main

