import StrToInt(makeIntList)

data Mn a = Mn a (a->a) 

instance Show a => Show (Mn a) where
  show (Mn t0 _) = show t0

instance Eq a => Eq (Mn a) where
  (==) (Mn t0 _) (Mn t1 _) = t0 == t1

(.>) :: Mn a -> Mn a -> Mn a
(.>) (Mn t0 y0) (Mn t1 y1) = Mn (y0 t1) y1 

getTai :: Mn a -> a
getTai (Mn t0 _) = t0

getYou :: Mn a -> (a->a)
getYou (Mn _ y) = y

numToMn :: Num a => a -> Mn a 
numToMn n = Mn n (n +)


--instance Functor Mn where
--  fmap :: (a -> b) -> Mn a -> Mn b
--  fmap f (Mn t y) = Mn (f t) (toFunc f y) 

--instance Applicative Mn where
--  pure :: a -> Mn a
--  pure x = Mn x id
--  (<*>) :: Mn (a -> b) -> Mn a -> Mn b 
--  mg <*> (Mn t y) = do
--    g <- mg
--    return (Mn (g t) (toFunc y g))

--instance Monad Mn where
--  (>>=) :: Mn a -> (a -> Mn b) -> Mn b
--  (Mn t _) >>= f = f t 

main :: IO ()
main = do
  siki <- getLine
  let numList = makeIntList siki
      mnList = map numToMn numList
      result = if null numList then 0 else getTai$foldl (.>) (numToMn 0) mnList 
  print result
  main

