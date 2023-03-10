colat :: Integer -> Integer
colat 1 = 1
colat n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1

colatList :: Integer -> [Integer]
colatList 1 = [1]
colatList n 
  | n < 1 = []
  | otherwise = n:(colatList (colat n))

colats :: [Integer]
colats = colats' 1
  where colats' :: Integer -> [Integer]
        colats' n = let l = length (colatList n)
                     in if (l>300) then [fromIntegral l]
                                   else fromIntegral l:colats' (n+1)
