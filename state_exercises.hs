{-# LANGUAGE InstanceSigs #-}
import System.Random (StdGen, randomR, mkStdGen)
import Data.Monoid

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
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
  deriving (Show)

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
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
rollsCountLoggedM n g = go 0 mempty g
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

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi fs) <*> (Moi gs) = Moi $ \s -> let (f, s') = fs s
                                          (a, s'') = gs s'
                                          b = f a
                                      in (b, s'')
