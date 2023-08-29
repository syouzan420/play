sosu :: Integer -> [Integer]
sosu 1 = [2]
sosu n = psosu++[wkazu (last psosu+1) psosu] 
   where psosu = sosu (n-1)

wkazu :: Integer -> [Integer] -> Integer
wkazu i nl = if warenai then i else wkazu (i+1) nl
  where warenai = all (\x -> i `mod` x/=0) nl

sosuat :: Integer -> Integer
sosuat = last.sosu

smai :: Integer -> [Integer]
smai n = map mai (sosu n)
  where mai num = let mozi = show num
                      ns = map readnum mozi
                      sm = sum ns
                   in if sm>9 then mai sm else sm
        readnum ch = read [ch] :: Integer

smod :: Integer -> Integer -> [Integer]
smod n m = map (`mod` m) (sosu n)
