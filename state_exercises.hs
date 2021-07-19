{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Random (StdGen, randomR, mkStdGen)
import Data.Monoid

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n  = count
      | otherwise =
        let (die, nextGen) =
             randomR (1, 6) gen
        in go (sum + die)
             (count + 1) nextGen

newtype Die = Die Int
  deriving (Show, Num)

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 0 []
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count dice gen
      | sum >= n  = (count, dice)
      | otherwise =
        let (dieInt, nextGen) =
             randomR (1, 6) gen
        in go (sum + dieInt) (count + 1) (dice ++ [Die dieInt]) nextGen

type Result = (Sum Int, [Die])

rollsCountLoggedM :: Int -> StdGen -> Result
rollsCountLoggedM n = go 0 mempty
  where
    go :: Int -> Result -> StdGen -> Result
    go sum result gen
      | sum >= n  = result
      | otherwise =
        let (dieInt, nextGen) =
             randomR (1, 6) gen
        in go (sum + dieInt) (result <> (Sum 1, [Die dieInt])) nextGen

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

evalMoi :: Moi s a -> s -> a
evalMoi m = fst . runMoi m

execMoi :: Moi s a -> s -> s
execMoi m = snd . runMoi m

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a,)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi fs) <*> (Moi gs) = Moi $ \s -> let (f, s') = fs s
                                          (a, s'') = gs s'
                                          b = f a
                                      in (b, s'')

instance Monad (Moi s) where
  return = pure
  
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi fs) >>= ga = Moi $ \s -> let (a, s') = fs s
                                    Moi bs = ga a
                                 in bs s'

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ const ((), s)
-- put = Moi . const . ((),)

roll :: Moi StdGen Int
roll = do
  gen <- get
  let (dieInt, nextGen) = randomR (1, 6) gen
  put nextGen
  return dieInt

rollsCountLoggedMoi :: Int -> StdGen -> Result
rollsCountLoggedMoi n = fst . runMoi(go 0 mempty)
  where
    go :: Int -> Result -> Moi StdGen Result
    go sum result
      | sum >= n  = return result
      | otherwise = do
          dieInt <- roll
          go (sum + dieInt) (result <> (Sum 1, [Die dieInt]))

--repl> rollsCountLoggedM 40 (mkStdGen 1233)
--repl> (Sum {getSum = 13},[Die 6,Die 4,Die 2,Die 4,Die 5,Die 1,Die 5,Die 4,Die 1,Die 2,Die 1,Die 2,Die 6])
--repl> (_, xs) = rollsCountLoggedM 40 (mkStdGen 1233)
--repl> sum (map (\(Die n) -> n) xs)
--repl> sum xs -- Tras derivar `Num` en `Die`

type RollM = Moi (StdGen, Result) 

roll' :: RollM Int 
roll' = do
  (gen, result) <- get
  let (dieInt, nextGen) = randomR (1, 6) gen
  put (nextGen, result)
  return dieInt

storeDie :: Int -> RollM ()
storeDie n = do
  (gen, result) <- get
  let result' = result <> (Sum 1, [Die n])
  put (gen, result')

rollsCountLoggedMoi' :: Int -> StdGen -> Result
rollsCountLoggedMoi' n gen = let state = execMoi (go 0) (gen, mempty)
                                 (gen, result) = state
                              in result
  where
    go :: Int -> RollM ()
    go sum
      | sum >= n  = return ()
      | otherwise = do
          dieInt <- roll'
          storeDie dieInt
          go (sum + dieInt)

-- TODO:
-- modify
-- getGen/getResult
-- putGen/putResult