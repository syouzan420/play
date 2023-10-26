module Syosu(showsu) where

showsu :: Double -> Int -> String
showsu db dg = let dbs = show db
                   (i,_:f) = break (=='.') dbs
                   ldg = length f
                   nf = if ldg <= dg then f else sgn (take (dg+1) f)
                   iup = head nf == '#'
                   ni = if iup then show (read i + 1) else i
                in ni ++ "." ++ (if iup then tail nf else nf) 

sgn :: String -> String
sgn dgs = let (ldg:xs) = reverse dgs 
              lnxs = length xs
              dg = read [ldg]::Int
              pf = (read$reverse xs)::Int
              i5u = dg > 4
              npf = show $ if i5u then pf+1 else pf
              lnpf = length npf 
           in if lnpf>lnxs then "#"++tail npf else npf

