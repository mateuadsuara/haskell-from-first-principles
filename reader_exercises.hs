import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

data FunT a b = Fun (a -> b)

instance Functor (FunT a) where
  fmap f (Fun g) = Fun (f . g)
  -- g :: a -> b
  -- f :: b -> c
  -- f . g :: a -> c
  -- Fun a c

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledm :: [Char] -> ([Char], [Char])
tupledm = do
  x <- cap
  y <- rev
  return (x, y)

tupledm' :: [Char] -> ([Char], [Char])
tupledm' =
  cap >>= (\x ->
  rev >>= (\y ->
  return (x, y)))

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

--f :: Reader Int Int
--f = do
--  r <- ask
--  return (r * 2)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader
