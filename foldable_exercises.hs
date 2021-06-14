import Data.Monoid (Sum(Sum, getSum), Product(Product, getProduct), Any(Any, getAny))
import Data.Semigroup (Min(Min, getMin), Max(Max, getMax))

sum' :: (Foldable t, Num a) => t a -> a
--sum' = foldr (+) 0
sum' = getSum . foldMap Sum
-- repl> sum' [1, 2, 3, 4] == 10

product' :: (Foldable t, Num a) => t a -> a
--product' = foldr (*) 1
product' = getProduct . foldMap Product
-- repl> product' [1, 2, 3, 4] == 24

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
--elem' a = foldr (\e acc -> acc || e == a) False
--elem' a = foldr ((||) . (== a)) False
elem' a = getAny . foldMap (\b -> Any (b == a))
--elem' a = getAny . foldMap (Any . (== a))
-- repl> (elem' 4 [1, 2, 3, 4] == True, elem' 5 [1, 2, 3, 4] == False)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
--minimum' = foldr f Nothing
--  where f e Nothing   = Just e
--        f e (Just e') = Just (min e e')
minimum' = (getMin <$>) . foldMap (Just . Min)
-- repl> let f = minimum' in (f [] == Nothing, f [2, 1] == Just 1, f [1, 2, 3] == Just 1)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = (getMax <$>) . foldr (\e acc -> Just (Max e) <> acc) Nothing
--maximum' = (getMax <$>) . foldMap (Just . Max)
-- repl> let f = maximum' in (f [] == Nothing, f [2, 1] == Just 2, f [1, 2, 3] == Just 3, f ["a", "b", "c"] == Just "c")

null' :: (Foldable t) => t a -> Bool
null' = not . getAny . foldMap (const (Any True))
-- repl> let f = null' in (f [] == True, f [1] == False, f [1..] == False)

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const (Sum 1))
-- repl> let f = length' in (f [] == 0, f [1] == 1, f [3, 1, 3] == 3, f (1, 2) == 1)

toList :: (Foldable t) => t a -> [a]
toList = foldMap (:[])
-- repl> let f = toList in (f [1, 2, 3] == [1, 2, 3], f (1, 2) == [2], f (Just 3) == [3], f Nothing == [])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id
-- repl> let f = fold' in (f [Sum 1, Sum 2, Sum 3] == Sum 6, f [Product 2, Product 3] == Product 6)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\e acc -> f e <> acc)  mempty
-- repl> let f = foldMap' in (f Sum [1, 2, 3] == Sum 6, f Product [2, 3] == Product 6)
