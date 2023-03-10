import Data.List(transpose)
import Myfile(fileRead)

mozisu = 15
fileName = "yoko.txt"

toTate :: String -> String
toTate = unlines.map (addSpace.reverse).transpose.concatMap (makeSameLength mozisu).lines

addSpace :: String -> String
addSpace [] = []
addSpace (x:xs) = let en = fromEnum x
                   in if en>10 && en<150 then ' ':x:addSpace xs
                                         else x:addSpace xs

makeSameLength :: Int -> String -> [String]
makeSameLength ms str =
  let sln = length str
   in if sln>ms then take ms str:makeSameLength ms (drop ms str)
                else [str++replicate (ms-sln) ' ']

main :: IO ()
main = do
  con <- fileRead fileName
  putStr $ toTate con
