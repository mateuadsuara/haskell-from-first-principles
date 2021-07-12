{-# LANGUAGE InstanceSigs #-}
import Data.Char
import Data.Maybe

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

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ (\r ->
    let ab = rab r
        a  = ra r
        b  = ab a
    in b)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> let a = ra r
                       Reader rb = aRb a
                       b = rb r
                   in b

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

--lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 (zip x y)

ys :: Maybe Integer
ys = lookup 6 (zip y z)

zs = lookup 4 (zip x y)

z' n = lookup n (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 = (,) <$> ys <*> zs

x3 n = (z' n, z' n)

--uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- && :: Bool -> Bool -> Bool
-- >3 :: Integer -> Bool
-- <8 :: Integer -> Bool
bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)
--bolt = runReader ((&&) <$> Reader (>3) <*> Reader (<8))

main :: IO ()
main = do
  let s' = summed <$> ((,) <$> xs <*> ys)
  print $ "s'"
  print $ s'
  print $ "xs"
  print $ xs
  print $ "ys"
  print $ ys
  print $ foldr (&&) True $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
  --print $
  --  sequenceA [Just 3, Just 2, Just 1]
  --print $ sequenceA [x, y]
  --print $ sequenceA [xs, ys]
  --print $ s'
  --print $ fmap summed ((,) <$> xs <*> zs)
  --print $ bolt 7
  --print $ fmap bolt z
  --print $ sequenceA [(>3), (<8), even] 7

sequA m = sequenceA [(>3), (<8), even] m
