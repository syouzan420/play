
newtype Mn a = Mn a

instance Show a => Show (Mn a) where
  show (Mn t) = show t

instance Eq a => Eq (Mn a) where
  (==) (Mn t0) (Mn t1) = t0 == t1

instance Functor Mn where
  fmap :: (a -> b) -> Mn a -> Mn b
  fmap f (Mn t) = Mn (f t)

instance Applicative Mn where
  pure :: a -> Mn a
  pure = Mn
  (<*>) :: Mn (a -> b) -> Mn a -> Mn b
  (<*>) (Mn f) (Mn t) = Mn (f t)

instance Monad Mn where
  return :: a -> Mn a
  return = pure
  (>>=) :: Mn a -> (a -> Mn b) -> Mn b
  (>>=) (Mn t) g = g t

class Mana a where
  mn :: a -> (a -> Mn a) 

instance Mana Int where
  mn n x = Mn (x + n)

instance Mana Integer where
  mn n x = Mn (x + n)

instance Mana String where
  mn n x = Mn (x <> n)

ns :: Show a => a -> Mn String
ns x = Mn (show x)
