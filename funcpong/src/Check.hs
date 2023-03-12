module Check (checkDef,checkEq,checkEv,trEvent) where

import Define (Grid,Def,Evt,State(..),Mode(Fr))
import Stages 
import Messages (msgs,msgR,msgW)
import Libs (isNum,isChar,getIndex)
import Grid
import Siki


checkDef :: [Def] -> Grid -> [Def]
checkDef df grid =
  let wheq [] = []
      wheq (g:gs) =
        let (chs,_) = unzip g
            ie = elem '=' chs
            (lf,rt)=if ie then sepeq chs else ([],[])
            (strs,exp) = unzip df
            idf = elem lf strs
            ex = exp!!(getIndex lf strs) 
            d = idef idf (lf,rt) 
         in if idf then (lf,ex):(wheq gs) else 
              if (ie&&(d>0)) then (checkLine d (lf,rt)):(wheq gs) else wheq gs
      idef idf (lf,rt) = 
        if (isChar lf && (not idf) && rt/="" && isNum rt) then 1 else 
        if (isChar rt && (not idf) && rt/="" && isNum rt) then 2 else 0
      checkLine i (lf,rt) =
        case i of
          1 -> (lf,read rt)
          2 -> (rt,read lf)
          _ -> ("",0) 
   in wheq grid

checkEq :: [Def] -> Grid -> Bool
checkEq def grid =
  let wheq [] = True 
      wheq (g:gs) =
        let (chs,_) = unzip g
            ie = elem '=' chs
         in if ie then (checkLine chs) && (wheq gs) else True && (wheq gs) 
      checkLine str =  
        let (lf,rt) = sepeq str
         in if((siki def lf)==(siki def rt)) then True else False
   in wheq grid 

rmsp :: String -> String
rmsp [] = []
rmsp (x:xs) = if(x==' ') then rmsp xs else x:rmsp xs

sepeq :: String -> (String,String)
sepeq [] = ([],[])
sepeq (x:xs) = if(x=='=') then ([],rmsp xs) 
                          else if(x==' ') then (nf,ns) else (x:nf,ns)
  where (nf,ns) = sepeq xs

checkEv :: Int -> String -> State -> [Evt] -> (String,Int)
checkEv _ _ _ [] = ([],0)
checkEv i lg st ((e,t):xs) =
  let iand = elem '&' e 
      id = if iand then getIndex '&' e else (-1) 
      ac = if iand then drop (id+1) e else ""
      ne = if iand then take id e else e
      (c:ns) = if iand then ac else "x"
      ic = case c of
             's' -> if(sn st==(read ns::Int)) then True else False
             _   -> True 
      le = length ne
      ll = drop (length lg - le) lg
      ne' = if(last ne=='?') then init ne else ne
      ll' = if(last ne=='?'&&ll/="") then init ll else ll 
   in if(ne'==ll'&&ic) then (t,i) else checkEv (i+1) lg st xs

trEvent :: Int -> String -> State -> State
trEvent id ev@(e:es) st =
  let cs = ecs st 
      c = cs!!id
      st' = 
        case e of
          'm' -> if(c>0) then st
                         else let mn = read es::Int
                                  ms = msgs!!mn
                               in st{msg=ms,ims=True}
          'r' -> let stn = sn st
                     igs = gridSize!!stn
                     (cid',cc') = if(es==[]) then ("","") else sepeq es
                     id' = if(cid'=="") then (-1) else read cid'::Int
                     c' = if(cc'=="") then (-1) else read cc'::Int
                     ecs' = if(c'==(-1)) 
                               then cs
                               else (take id' cs) ++ [c'] ++ (drop (id'+1) cs)
                  in st{xy=initPos!!stn,sz=igs,
                   gr=makeGrid igs (stages!!stn),pl=player!!stn,et=' ',
                   sn=stn,ecs=ecs',msg=msgR,ims=True}
          'w' -> let igs = head gridSize
                     (cid',cc') = if(es==[]) then ("","") else sepeq es
                     id' = if(cid'=="") then (-1) else read cid'::Int
                     c' = if(cc'=="") then (-1) else read cc'::Int
                     ecs' = if(c'==(-1))
                               then cs
                               else (take id' cs) ++ [c'] ++ (drop (id'+1) cs)
                  in st{xy=initPos!!0,sz=igs,
                   gr=makeGrid igs stage0,pl=head player,et=' ',
                   sn=0,ecs=ecs',msg=msgW,ims=True}
          'a' -> let (ch:po) = es
                     (cx,cy) = sepeq po
                     (x,y) = (read cx,read cy)
                     ngrid = intoGrid (x,y) (ch,Fr) (gr st) 
                  in st{gr=ngrid}
          's' -> st{gr=toSee$gr st}
          'h' -> st{gr=toHide$gr st}
          _   -> st
      cs' = ecs st'
      necs = (take id cs') ++ [c+1] ++ (drop (id+1) cs')
      (tgt,id') = checkEv 0 ("t"++(show id)++"_"++(show (c+1))) st' evts
      nst = if (tgt==[]) then st' else trEvent id' tgt st'
   in nst{ecs=necs,elg=elg st++ev}

