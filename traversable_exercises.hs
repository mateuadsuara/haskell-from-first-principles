import Data.Monoid (Sum(Sum))
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity i) = Identity $ f i

instance Foldable Identity where
  foldMap f (Identity i) = f i

instance Traversable Identity where
  --traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f (Identity i) = Identity <$> f i

--repl> let t = traverse in (t (\x -> Just x) (Identity 1) == Just (Identity 1), t (\x -> [x, x-1]) (Identity 1) == [Identity 1, Identity 0])

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap _ (Constant x) = (Constant x)

instance Foldable (Constant a) where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap _ (Constant x) = mempty

instance Traversable (Constant c) where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse _ (Constant k) = pure (Constant k)

--repl> let t = traverse; c = Constant 1 in (t (\x -> Just x) c == Just c, t (\x -> [x, x-1]) c == [c])

data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap f Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

--repl> let t = traverse in (t (\x -> [x]) (Yep 4) == [Yep 4], t (\x -> [x, x-1]) (Yep 3) == [Yep 3, Yep 2], t id Nada == Just Nada)

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap f Nil = mempty
  foldMap f (Cons a l) = f a <> foldMap f l --repl> foldMap (\x -> [x]) (Cons 1 (Cons 2 Nil)) == [1, 2]
  --foldMap f (Cons a l) = foldMap f l <> f a --repl> foldMap (\x -> [x]) (Cons 1 (Cons 2 Nil)) == [2, 1]

instance Traversable List where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f Nil = pure Nil
  traverse f (Cons a l) = Cons <$> f a <*> traverse f l

--repl> let t = traverse in (t (\x -> Just (x - 1)) Nil == Just Nil, t (\x-> Just (x - 1)) (Cons 5 Nil) == Just (Cons 4 Nil))

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f (Three x y z) = Three x y <$> f z

--repl> let t = traverse in (t (\x -> Just x) (Three "a" Nothing 1) == Just (Three "a" Nothing 1), t (\x -> [x, x-1]) (Three "a" Nothing 1) == [Three "a" Nothing 1, Three "a" Nothing 0])

data Big a b = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f (Big x y z) = Big x <$> f y <*> f z

--repl> let t = traverse in (t (\x -> Just (x + 1)) (Big "a" 4 8) == Just (Big "a" 5 9))

data S n a = S (n a) a
  deriving (Eq, Show)


instance Functor n => Functor (S n) where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (S x y) = S (fmap f x) (f y)
--repl> fmap (\i -> i + 1) (S (Just 1) 3) == S (Just 2) 4

instance Foldable n => Foldable (S n) where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap f (S x y) = foldMap f x <> f y
--repl> foldMap (\i -> Sum (i + 1)) (S (Just 1) 3) == Sum 6

instance Traversable n => Traversable (S n) where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f (S x y) = S <$> traverse f x <*> f y
--repl> let t = traverse in (t (\i -> Just (i + 1)) (S [1, 2] 3) == Just (S [2, 3] 4), t (\i -> [i + 1, i + 2]) (S (Just 1) 3) == [S (Just 2) 4,S (Just 2) 5,S (Just 3) 4,S (Just 3) 5])

instance (Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
  , Testable (n Property)
  , Eq a
  , Eq (n a)
  , EqProp a) => EqProp (S n a) where
  (=-=) = eq

main = do
  let trigger :: S [] (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
--fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f Leaf = Leaf
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Foldable Tree where
--foldMap :: (Foldable t, Monoid m) => (b -> m) -> t b -> m
  foldMap f Leaf = mempty
  foldMap f (Node a l r) = f a <> foldMap f l <> foldMap f r

instance Traversable Tree where
--traverse :: (Applicative a, Traversable t) => (x -> a y) -> t x -> a (t y)
  traverse f Leaf = pure Leaf
  traverse f (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r
--repl> let t = traverse in (t (\i -> Just (i + 1)) (Node 1 Leaf Leaf), t (\i -> [i + 1, i + 2]) (Node 2 Leaf Leaf), t (\i -> Just (i + 1)) (Node 1 (Node 2 Leaf Leaf) Leaf), t (\i -> [i + 1, i + 2]) (Node 2 (Node 3 Leaf Leaf) Leaf))
