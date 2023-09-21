--btos -- binary data to sin function

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

import Data.Char (digitToInt)
import Numeric(showBin)

type Bi = [Bool]

type Phase = Double 
type Cycle = Double 
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

isPeriodic :: Int -> Bi -> Bool
isPeriodic _ [] = True
isPeriodic x bls = head bls && isPeriodic x (drop x bls)

nextPTrue :: Int -> Bi -> Integer
nextPTrue x bls = if isPeriodic x bls then x else nextPTrue (x+1) bls

pc :: Int -> Bi -> PC 
pc i bls
  | length bls - 1 < i = PC (-1) 0
  | bls!!i = PC (fromIntegral i) (nextPTrue (i+1) (drop i bls))
  | otherwise = pc (i+1) bls

pcs :: Bi -> [PC]
pcs bls = let pnc = pc 0 bls
              nbl = nextBoolList pcls bls
           in if pnc==PC (-1) 0 then [] else pnc:pcs nbl

nextBoolList :: PC -> Bi -> Bi
nextBoolList = changeTF 0

changeTF :: Int -> PC -> Bi -> Bi 
changeTF _ _ [] = []
changeTF x (PC p c) bls
  | x > length bls - 1 = []
  | x == ip = False:nextTF 
  | x `mod` ic == ip = False:nextTF
  | otherwise = (bls!!x):nextTF
  where nextTF = changeTF (x+1) (PC p c) bls
        ip = floor p
        ic = floor c

binToBfunc :: Bi -> (Int -> Bool)
binToBfunc b i = foldr ((<+>).makeBool) [] (pcs b)!!i

intvToBool :: Int -> [PC] -> Bi
intvToBool i pcls = take i (foldr ((<+>).makeBool) [] pcls)

(<+>) :: Bi -> Bi -> Bi
(<+>) [] bls = bls 
(<+>) bls [] = bls 
(<+>) (b0:b0s) (b1:b1s) = (b0||b1):(b0s <+> b1s)

makeBool :: PC -> Bi
makeBool (PC p c) = 
  replicate (floor p) False ++ cycle (True:replicate (floor c-1) False)

binToCos :: Bi -> (Double -> Double)
binToCos b = toCos.pcs

toCos :: [PC] -> (Double -> Double)
toCos pcls x = foldl (\acc (PC p c) -> acc+cos (((2*pi)/c)*(x-p))) 0 pcls 

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
