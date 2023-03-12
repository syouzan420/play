module Loop (initiate,inputLoop) where

import Define
import Stages
import Grid
import Messages
import Terminal
import OutToTerm
import Check
import Siki
import Libs

initState :: State
initState = State {xy=head initPos,sz=igs,
                   gr=makeGrid igs (head stages),pl=head player,et=' ',
                   sn=0,msg=initMsg,elg="",ecs=replicate (length evts) 0,
                   isc=False,igc=False,ims=False,db="",ch=' '}
              where igs = head gridSize

keyCheck :: Size -> Pos -> Char -> Pos 
keyCheck (wd,hi) (x,y) ch  
  | ch=='j'||ch=='P' = if(hi==y+1) then (x,y) else (x,y+1)
  | ch=='k'||ch=='H' = if(0>y-1) then (x,y) else (x,y-1)
  | ch=='h'||ch=='K' = if(0>x-1) then (x,y) else (x-1,y)
  | ch=='l'||ch=='M' = if(wd==x+1) then (x,y) else (x+1,y)
  | otherwise = (x,y) 

fpon :: Char -> Char -> Char
fpon p e = let (cs,fs)=unzip funcPon 
               f = fs!!(getIndex p cs)
            in f e

spon :: Int -> Char -> Char -> Char
spon i e t = let (sn,fs)=unzip stagePon
                 f = if(elem i sn) then fs!!(getIndex i sn) else doNothing
              in f e t

putOut :: State -> State
putOut st =
  let (pla,grid,pos,snum,eat,lg) = (pl st,gr st,xy st,sn st,et st,elg st)
      (ch,tp) = fromGrid pos grid 
      iout = eat/=' ' && ch==' '
      ipn = tp==Pn
      eat' = if iout then ' ' else eat
      ch' = if iout then fpon pla eat else 
              if ipn then spon snum (fpon pla eat) ch else ch
      grid' = intoGrid pos (ch',tp) grid 
      lg' = lg++(if iout then 'o':[ch'] else if ipn then 'p':[ch'] else "")
      def = checkDef idef grid'
      isc' = checkEq def grid'
   in st{gr=grid',et=eat',isc=isc',elg=lg'}

plMove :: Pos -> State -> State
plMove (x,y) st = 
  let (pla,eat,grid,mpos,lg)=(pl st,et st,gr st,xy st,elg st)
      (ch,tp) = fromGrid (x,y) grid 
      fps = fst$unzip funcPon
      ifp = eat==' ' && (elem ch fps)
      ieat = eat==' ' && ch/=' ' && (tp==Fr||tp==DF) && (not ifp)
      eat' = if ieat then ch else eat
      pla' = if ifp then ch else pla 
      ch' = if ieat then ' ' else if ifp then pla else ch
      grid' = intoGrid (x,y) (ch',tp) grid
      imv = tp==Mv || (tp==Pn && eat==' ')
      ngrid = if imv then obMove mpos (x,y) grid' else grid'
      (x',y')=if((imv&&ngrid==grid')||tp==Bl||tp==DB) then mpos else (x,y)
      isc' = if imv then checkEq (checkDef idef ngrid) ngrid else False
      lg' = lg++(if ieat then 'e':[ch] else if ifp then 'f':[ch] else
                  if (imv&&mpos/=(x',y')) then 'v':[ch] else
                    if (tp==Pn) then 'n':[ch] else "")
   in st{xy=(x',y'),gr=ngrid,pl=pla',et=eat',isc=isc',elg=lg'} 

mkDir :: Pos -> Pos -> Dir
mkDir (x0,y0) (x1,y1)
  |x1>x0 = Rt
  |x0>x1 = Lf
  |y1>y0 = Dw
  |y0>y1 = Up
  |otherwise = Cn

obMove :: Pos -> Pos -> Grid -> Grid
obMove mpos (x,y) grid =
  let gline = grid!!y
      (ch,tp) = gline!!x 
      ngrid = intoGrid (x,y) (' ',Fr) grid 
      dir = mkDir mpos (x,y)
      (x',y') = case dir of
                  Up -> if(y==0) then ((-1),(-1)) else (x,y-1)
                  Dw -> if(y==length grid-1) then ((-1),(-1)) else (x,y+1)
                  Lf -> if(x==0) then ((-1),(-1)) else (x-1,y)
                  Rt -> if(x==length gline-1) then ((-1),(-1)) else (x+1,y)
      tgline = if(y'==(-1)) then [] else ngrid!!y'
      (tch,_) = if(x'==(-1)) then (' ',Fr) else tgline!!x'
      imv = x'/=(-1) && tch==' '
      ngline' = (take x' tgline)++[(ch,tp)]++(drop (x'+1) tgline) 
   in if imv then take y' ngrid ++[ngline']++ drop (y'+1) ngrid
             else grid

initiate :: IO State 
initiate = do
  let (x,y)=xy initState
      (wd,hi)=sz initState
      (px,py)=(x+ix,y+iy)
  putGrid (ix,iy) (gr initState)
  putMessage (ix,iy+hi+2) (msg initState)
  chColor1 >> putMozi (px+1,py+1) "@" >> mkDefault
  return initState 

inputLoop :: State -> IO ()
inputLoop st = do
  i <- getMozi
  let (wd,hi) = sz st
      (x,y) = xy st
      (x',y') = keyCheck (wd,hi) (x,y) i
      st' = plMove (x',y') st 
      (tx,ty) = xy st'
      (px,py) = (x+ix,y+iy)
      (px',py') = (tx+ix,ty+iy)
      st'' = if i==' ' then putOut st' else st'
  putGrid (ix,iy) (gr st'')
  let (tgt,id) = checkEv 0 (elg st'') st'' evts
      nst = if (tgt==[]) then st'' else trEvent id tgt st''
  if (ims nst) then putMessage (ix,iy+hi+2) (msg nst) else return () 
  if (isc nst) then nextStage nst{ims=False} 
                else do
        let pxy = (px'+1,py'+1)
        if(et nst==' ') then chColor1 >> putMozi pxy [pl nst] >> mkDefault
                        else chColor2 >> putMozi pxy [pl nst] >> mkDefault
        if i=='\ESC' then do
                        putGrid (ix,iy) (gr st'')
                        putMoziLn (ix,iy+hi+2) "Are You Sure To QUIT? (y/n)"
                        q <- getMozi
                        if(q=='y') then return ()
                                   else inputLoop nst{ims=False}
                     else inputLoop nst{ims=False}

nextStage :: State -> IO ()
nextStage st = do
  let nsn = sn st + 1
      maxSn = length stages
      gc = nsn == maxSn
      nlg = elg st++('c':(show (sn st)))
  if gc then gameClear
        else do
          let nsz=gridSize!!nsn
          inputLoop st{xy=initPos!!nsn,sz=nsz,gr=makeGrid nsz (stages!!nsn),
                       pl=player!!nsn,et=' ',sn=nsn,elg=nlg,isc=False,igc=False}

gameClear :: IO ()
gameClear = do putMoziLn (0,0) (show (10^10^5))
               putMessage (0,20) msgE
               cls
               putMoziLn (10,5) "Congratulations!"
               putMoziLn (13,8) "Coding : yokoP"
               putMoziLn (13,10) "Test Play : takaPon"
               putMoziLn (16,14) "I spent a special week, 10/18 to 10/25 in 2021"
               putMoziLn (10,16) "Thank you for playing!"

