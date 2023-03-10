main :: IO ()
main = do
  ln <- getLine
  mapM_ putStrLn (makeTate$reverse$makeSameLength$words ln)
  
getMaxLength :: [String] -> Int
getMaxLength strs = maximum$map length strs

addSpace :: Int -> String -> String
addSpace dl str = str++replicate (dl-length str) '\12288'
                  
makeSameLength :: [String] -> [String]
makeSameLength str = map (addSpace$getMaxLength str) str
                      
makeTate :: [String] -> [String]
makeTate strs
  | null (head strs) = []
  | otherwise = map head strs : makeTate (map tail strs)
