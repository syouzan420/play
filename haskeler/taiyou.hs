import StrToInt(makeIntList)

data Mn a = Mn a (a->a) 

(.>) :: Mn a -> Mn a -> Mn a
(.>) (Mn t0 y0) (Mn t1 y1) = Mn (y0 t1) y1 

getTai :: Mn a -> a
getTai (Mn t0 _) = t0

getYou :: Mn a -> (a->a)
getYou (Mn _ y) = y

intToMn :: Integer -> Mn Integer
intToMn i = Mn i (i +)

--toFunc :: (a -> a) -> (a -> b) -> (b -> b)
--toFunc (a0 -> a1) f = f a0 -> f a1

--instance Functor Mn where
--  fmap :: (a -> b) -> Mn a -> Mn b
--  fmap g (Mn t y) = Mn (g t) (toFunc y g) 

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
  let intList = makeIntList siki
      mnList = map intToMn intList
      result = if null intList then 0 else getTai$foldl (.>) (intToMn 0) mnList 
  print result
  main

