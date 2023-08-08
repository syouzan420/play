--btos -- binary data to sin function

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

main = toFile def "btos.svg" $ do
  layout_title .= "binary data to cos function "++show test++" "++show (intervals testb)
  setColors [opaque blue, opaque red]
  plot (line (show funcs) [zip testxs testys])


tb :: [Int] -> [Bool]
tb = map (>0)

bt :: [Bool] -> [Int]
bt = map (\b -> if b then 1 else 0)

nextTrue :: [Bool] -> Integer
nextTrue [] = 0
nextTrue (True:xs) = 1
nextTrue (False:xs) = 1 + nextTrue xs

isPeriodic :: Integer -> [Bool] -> Bool
isPeriodic _ [] = True
isPeriodic x bls = let xi = fromIntegral x 
                    in head bls && isPeriodic x (drop xi bls)

nextPTrue :: Integer -> [Bool] -> Integer
nextPTrue x bls = if isPeriodic x bls then x else nextPTrue (x+1) bls

interval :: Integer -> [Bool] -> (Integer,Integer)
interval i bls
  | length bls - 1< fromIntegral i = (-1,0)
  | bls!!fromIntegral i = (i,nextPTrue 1 (drop (fromIntegral i) bls))
  | otherwise = interval (i+1) bls

intervals :: [Bool] -> [(Integer,Integer)]
intervals bls = let intv = interval 0 bls
                    nbl = nextBoolList intv bls
                 in if intv==(-1,0) then [] else intv:intervals nbl

nextBoolList :: (Integer,Integer) -> [Bool] -> [Bool]
nextBoolList = changeTF 0

changeTF :: Integer -> (Integer,Integer) -> [Bool] -> [Bool] 
changeTF _ _ [] = []
changeTF x (fsi,inv) bls
  | fromIntegral x > length bls - 1 = []
  | x `mod` inv == fsi || (fsi>inv && (x-fsi-1) `mod` inv == 0) || (fsi==inv && (x-fsi) `mod` inv == 0)
                                  = False:changeTF (x+1) (fsi,inv) bls
  | otherwise = (bls!!fromIntegral x):changeTF (x+1) (fsi,inv) bls

intvToBool :: Integer -> [(Integer,Integer)] -> [Bool]
intvToBool i intvs = take (fromIntegral i) (intvToBool' intvs)

intvToBool' :: [(Integer,Integer)] -> [Bool]
intvToBool' = foldr ((<+>).makeBool) []  

(<+>) :: [Bool] -> [Bool] -> [Bool]
(<+>) [] bls = bls 
(<+>) bls [] = bls 
(<+>) (b0:b0s) (b1:b1s) = (b0||b1):(b0s <+> b1s)

makeBool :: (Integer,Integer) -> [Bool]
makeBool (fsi,inv) = 
  replicate (fromIntegral fsi) False ++ cycle (True:replicate (fromIntegral inv-1) False)

binToCos :: [Bool] -> (Double -> Double)
binToCos b = let intvs = intervals b
              in toCos intvs

toCos :: [(Integer,Integer)] -> (Double -> Double)
toCos invs x = foldl (\acc (fsi,inv) -> let fi=fromIntegral fsi; iv=fromIntegral inv in acc+(1/iv)^2*cos (((2*pi)/iv)*(x-fi))^2) 0 invs


test :: [Int]
test = [1,0,0,1,1,0,0,1,0,1,0,1,1,0,1,0,0,0,1,1,0,1,0,1,1,0]

testb :: [Bool]
testb = tb test

testfn :: Double -> Double
testfn = binToCos testb

testxs :: [Double]
testxs = [0::Double,0.1..25]

testys :: [Double]
testys = map testfn testxs

funcs :: String
funcs = "(1/iv)^2*cos (((2*pi)/iv)*(x-fi))^2"
