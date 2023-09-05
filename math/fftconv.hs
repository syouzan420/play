import Data.Complex

isPowerOfTwo :: Int -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo x
  | even x = isPowerOfTwo (div x 2)
  | otherwise = False

fftRaw :: (RealFloat a) => [a] -> Int -> Int -> [Complex a]
fftRaw _ 0 _ = []
fftRaw [] _ _ = []
fftRaw (x0:_) 1 _ = [x0 :+ 0]
fftRaw x n s = zipWith (+) x1 x2 ++ zipWith (-) x1 x2
  where x1 = fftRaw x (div n 2) (2 * s)
        x2 = zipWith (*) 
              [exp (0 :+ (-2 * pi * fromIntegral k / fromIntegral n)) | k <- [0..(div n 2 - 1)]]
                    (fftRaw (drop s x) (div n 2) (2 * s))
