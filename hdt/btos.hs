--btos -- binary data to sin function

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

import Data.Char (digitToInt)
import Numeric(showBin)

type Bi = [Bool]

type Phase = Integer
type Cycle = Integer
data PC = PC Phase Cycle deriving Eq

instance Show PC where
  show (PC p c) = show p ++ "("++show c++")" 

main = toFile def "btos.svg" $ do
  layout_title .= "binary data to cos function "++show test++" "++show (pcs testb)
  setColors [opaque blue, opaque red]
  plot (line (show funcs) [zip testxs testys])


tb :: String -> Bi 
tb = map ((>0).digitToInt)

bt :: Bi -> String 
bt = map (\b -> if b then '1' else '0')

nextTrue :: Bi -> Integer
nextTrue [] = 0
nextTrue (True:xs) = 1
nextTrue (False:xs) = 1 + nextTrue xs

isPeriodic :: Integer -> Bi -> Bool
isPeriodic _ [] = True
isPeriodic x bls = let xi = fromIntegral x 
                    in head bls && isPeriodic x (drop xi bls)

nextPTrue :: Integer -> Bi -> Integer
nextPTrue x bls = if isPeriodic x bls then x else nextPTrue (x+1) bls

pc :: Integer -> Bi -> PC 
pc i bls
  | length bls - 1< fromIntegral i = PC (-1) 0
  | bls!!fromIntegral i = PC i (nextPTrue 1 (drop (fromIntegral i) bls))
  | otherwise = pc (i+1) bls

pcs :: Bi -> [PC]
pcs bls = let intv = pc 0 bls
              nbl = nextBoolList intv bls
           in if intv==PC (-1) 0 then [] else intv:pcs nbl

nextBoolList :: PC -> Bi -> Bi
nextBoolList = changeTF 0

changeTF :: Integer -> PC -> Bi -> Bi 
changeTF _ _ [] = []
changeTF x (PC fsi inv) bls
  | fromIntegral x > length bls - 1 = []
  | x == fsi = False:changeTF (x+1) (PC 0 inv) bls 
  | x `mod` inv == fsi = False:changeTF (x+1) (PC fsi inv) bls
  | otherwise = (bls!!fromIntegral x):changeTF (x+1) (PC fsi inv) bls

binToBfunc :: Bi -> (Integer -> Bool)
binToBfunc b i = let intvs = pcs b
                  in intvToBool' intvs!!fromIntegral i

intvToBool :: Integer -> [PC] -> Bi
intvToBool i intvs = take (fromIntegral i) (intvToBool' intvs)

intvToBool' :: [PC] -> Bi
intvToBool' = foldr ((<+>).makeBool) []  

(<+>) :: Bi -> Bi -> Bi
(<+>) [] bls = bls 
(<+>) bls [] = bls 
(<+>) (b0:b0s) (b1:b1s) = (b0||b1):(b0s <+> b1s)

makeBool :: PC -> Bi
makeBool (PC fsi inv) = 
  replicate (fromIntegral fsi) False ++ cycle (True:replicate (fromIntegral inv-1) False)

binToCos :: Bi -> (Double -> Double)
binToCos b = let intvs = pcs b
              in toCos intvs

toCos :: [PC] -> (Double -> Double)
toCos invs x = foldl (\acc (PC fsi inv) -> let fi=fromIntegral fsi; iv=fromIntegral inv in acc+(1/iv)^2*cos (((2*pi)/iv)*(x-fi))^2) 0 invs


test :: String 
test = "10011001010110100011010110"

test2 :: String 
test2 = "10011010110"

test3 :: String 
test3 = "110110001010011101001011010111011010110001011010110101101010100111011011010"

testb :: Bi
testb = tb test

testfn :: Double -> Double
testfn = binToCos testb

testxs :: [Double]
testxs = [0::Double,0.1..25]

testys :: [Double]
testys = map testfn testxs

funcs :: String
funcs = "(1/iv)^2*cos (((2*pi)/iv)*(x-fi))^2"
