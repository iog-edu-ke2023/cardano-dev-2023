{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Probability where

import           Control.Monad      (ap, liftM)
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           System.Random      (randomRIO)

class Monad m => MonadProb m where
    coin :: Rational -> m Bool -- coin p should represent flipping a coin with probability p for True and (1-p) for False.

-- | randomInt n should give a random in in 0, 1, ... n
randomInt :: MonadProb m => Int -> m Int
randomInt n
    | n <= 0    = pure 0
    | otherwise = do
        b <- coin $ 1 / (fromIntegral n + 1)
        if b
            then pure 0
            else (+ 1) <$> randomInt (n - 1)

die :: MonadProb m => m Int
die = (+ 1) <$> randomInt 5

pick :: MonadProb m => NonEmpty a -> m a
pick (x :| xs) = do
    i <- randomInt $ length xs
    pure $
        if i == 0
            then x
            else xs !! (i - 1)

twoDice :: MonadProb m => m Int
twoDice = (+) <$> die <*> die

instance MonadProb IO where
    coin :: Rational -> IO Bool
    coin p = do
        x <- randomRIO (0, 1)
        pure $ x < p'
      where
        p' :: Double
        p' = fromRational p

newtype Dist a = Dist [(a, Rational)] -- invariant: all rationals are in [0, 1] and add up to 1.
    deriving Show

instance Functor Dist where
    fmap :: (a -> b) -> Dist a -> Dist b
    fmap = liftM

instance Applicative Dist where
    pure :: a -> Dist a
    pure a = Dist [(a, 1)]

    (<*>) :: Dist (a -> b) -> Dist a -> Dist b
    (<*>) = ap

instance Monad Dist where
    (>>=) :: Dist a -> (a -> Dist b) -> Dist b
    Dist xs >>= k = Dist $ do -- in the list-monad!!!
        (a, p) <- xs
        let Dist ys = k a
        [(b, p * q) | (b, q) <- ys]

instance MonadProb Dist where
    coin :: Rational -> Dist Bool
    coin p = Dist [(True, p'), (False, 1 - p')]
      where
        p' = max 0 $ min 1 p

summarize :: forall a. Ord a => Dist a -> Map a Rational
summarize (Dist xs) = foldl' f Map.empty xs
  where
    f :: Map a Rational -> (a, Rational) -> Map a Rational
    f m (a, p) = Map.insertWith (+) a p m

data Strategy = Stay | Change
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

montyHall :: MonadProb m => Strategy -> m Bool
montyHall s = do
    carIndex    <- randomInt 2
    firstChoice <- randomInt 2
    hostOpens   <- pick $ NE.fromList [i | i <- [0, 1, 2], i /= firstChoice, i /= carIndex]
    let finalChoice = case s of
            Stay   -> firstChoice
            Change -> head [i | i <- [0, 1, 2], i /= firstChoice, i /= hostOpens]
    pure $ finalChoice == carIndex
