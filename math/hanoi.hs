data Peg = A | B | C deriving (Eq,Show)
type Source = Peg
type Distination = Peg
type Temporal = Peg
type Disks = Integer
data Move = Move Peg Peg deriving Eq

instance Show Move where
  show (Move a b) = show a ++ "=>" ++ show b

hanoi :: Disks ->  Source -> Distination -> Temporal -> [Move]
hanoi 1 s d t = [Move s d]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a 

