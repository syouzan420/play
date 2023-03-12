module Define where

type Pos = (Int,Int)
type Mana = Char 
type Cell = (Mana,Mode)
type Grid = [[Cell]]
type Stage = [(Pos,Cell)]
type Size = (Int,Int)
type Fpon = (Char,Func)
type Spon = (Int,Func2)
type Func = (Char->Char)
type Func2 = (Char->Char->Char)
type Def = (String,Integer)
type Msg = [String]
type Evt = (String,String)    --(Triger Event, Target Event)


data Mode = Fr | Bl | Mv | Pn | DB | DF deriving (Eq,Show)

data Dir = Up | Dw | Lf | Rt | Cn deriving (Eq,Show)

data State = State {xy::Pos,
                    sz::Size,
                    gr::Grid,
                    pl::Char,     -- Players Appearance
                    et::Mana,     -- That Player Eats
                    sn::Int,      -- Stage Number
                    msg::[String],-- Messages
                    elg::String,  -- eventLog
                    ecs::[Int],   -- Event Counts
                    isc::Bool,    -- Stage Clear?
                    igc::Bool,    -- Game Clear?
                    ims::Bool,     -- Show Message?
                    db::String,    --for debug
                    ch::Char       --optional
                   } deriving (Eq,Show)

ix, iy :: Int
ix = 5; iy = 2

idef :: [Def]
idef = [("PASS",8720),("HUGE",10^10^5)]

funcPon :: [Fpon]
funcPon = [('@',id),('A',addone),('S',subone)]

stagePon :: [Spon]
stagePon = [(5,makeShift)]

addone :: Char -> Char
addone = makeNext 1

subone :: Char -> Char
subone = makeNext (-1)

makeNext :: Int -> Char -> Char
makeNext i ch
  |elem ch "0123456789" = let tn = (read [ch]::Int)+i
                           in if(tn==10) then '9'
                                         else if(tn==(-1)) then '0'
                                            else last$show tn
  |otherwise = let en = fromEnum ch
                   en' = en+i
                   nen = if(en'>122) then 97+(en'-122) else
                            if(en'<97 && en'>90) then 65+(en'-90) else en'
                   tc = toEnum nen::Char
                in if (elem tc (fst$unzip funcPon))
                      then makeNext i tc
                      else tc

makeShift :: Char -> Char -> Char
makeShift ic ch = if (elem ic "0123456789") then makeNext (read [ic]) ch
                                            else ch

doNothing :: Char -> Char -> Char
doNothing _ ch = ch

