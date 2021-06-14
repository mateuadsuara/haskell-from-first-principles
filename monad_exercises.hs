j :: Monad m => m (m a) -> m a
--j mma = mma >>= \ma -> ma
--j = (>>= id)
j mma = do
  ma <- mma
  ma
-- repl> (j [[1, 2], [], [3]], j (Just (Just 1)), j (Just Nothing))

l1 :: Monad m => (a -> b) -> m a -> m b
--l1 f ma = ma >>= \a -> return $ f a
--l1 f = (>>= return . f)
l1 f ma = do
  a <- ma
  return $ f a
-- repl> (l1 odd (Just 1), l1 odd Nothing, l1 odd [1, 2, 3])


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--l2 f2 ma mb = ma >>= \a -> mb >>= \b -> return $ f2 a b
--l2 f2 ma mb = ma >>= \a -> l1 (f2 a) mb
l2 f2 ma mb = do
  a <- ma
  b <- mb
  return $ f2 a b
-- repl> (l2 (<) (Just 1) (Just 2), l2 (<) Nothing (Just 2), l2 (<) [1, 2, 3] [3, 2, 1])

a :: Monad m => m a -> m (a -> b) -> m b
--a ma mf = ma >>= \a' -> mf >>= \f -> return $ f a'
--a ma mf = mf <*> ma
--a = flip (<*>)
a ma mf = do
  a' <- ma
  f <- mf
  return $ f a'
-- repl> (a (Just 4) (Just odd), a [1, 2, 3] [odd, (<2)])

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
--meh (x:xs) f = f x >>= \b -> meh xs f >>= \bs -> return $ b : bs

meh (x:xs) f = do
  b <- f x -- Primero hace los efectos secundarios de la cabeza de la lista
  bs <- meh xs f
  return $ b : bs
-- repl> meh [1, 2, 3] (\x -> print x >> return x)
-- 1
-- 2
-- 3
-- [1,2,3]

--meh (x:xs) f = do
--  bs <- meh xs f -- Primero hace los efectos secundarios del resto de la lista
--  b <- f x
--  return $ b : bs
-- repl> meh [1, 2, 3] (\x -> print x >> return x)
-- 3
-- 2
-- 1
-- [1,2,3]

flipType :: Monad m => [m a] -> m [a]
--flipType mas = meh mas id
flipType = (`meh` id)
-- repl> (flipType [Just 2, Just 3, Just 4])


