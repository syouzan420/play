import Data.List (transpose)

pmatrix :: Num a => [a] -> [a] -> [[a]]
pmatrix a b = [[ x*y | y <- b] | x <- a]

zeros :: Num a => Int -> [[a]]
zeros 0 = [[]]
zeros n = zeros (n-1) ++ [replicate n 0] 

add0s :: Num a => [[a]] -> [[a]]
add0s pm = zipWith (++) (zeros (length pm - 1)) pm

addCol :: Num a => [[a]] -> [a]
addCol pm0 = map sum (transpose pm0)

convolve :: Num a => [a] -> [a] -> [a]
convolve a b = addCol$add0s$pmatrix a b
