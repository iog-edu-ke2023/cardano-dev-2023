{-# LANGUAGE DeriveFunctor #-}

module FreeMonads.Prob where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           FreeMonads.Free
import           System.Random      (randomRIO)

data ProbOp a = Coin Rational (Bool -> a)
    deriving Functor

type Prob = Free ProbOp

coin :: Rational -> Prob Bool
coin p = Free $ Coin p pure

-- | randomInt n should give a random in in 0, 1, ... n
randomInt :: Int -> Prob Int
randomInt n
    | n <= 0    = pure 0
    | otherwise = do
        b <- coin $ 1 / (fromIntegral n + 1)
        if b
            then pure 0
            else (+ 1) <$> randomInt (n - 1)

pick :: NonEmpty a -> Prob a
pick (x :| xs) = do
    i <- randomInt $ length xs
    pure $
        if i == 0
            then x
            else xs !! (i - 1)

die :: Prob Int
die = succ <$> randomInt 5

twoDice :: Prob Int
twoDice = (+) <$> die <*> die

probToIO :: Prob a -> IO a
probToIO (Pure a) = pure a
probToIO (Free (Coin p k)) = do
    x <- randomRIO (0, 1)
    let b = x < p'
    probToIO $ k b
  where
    p' :: Double
    p' = fromRational p

probPure :: Ord a => Prob a -> Map a Rational
probPure (Pure a)          = Map.singleton a 1
probPure (Free (Coin p k)) =
  let
    dTrue  = probPure $ k True
    dFalse = probPure $ k False
  in
    Map.unionWith (+) ((p *) <$> dTrue) (((1 - p) *) <$> dFalse)

data Strategy = Stay | Change
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

montyHall :: Strategy -> Prob Bool
montyHall s = do
    carIndex    <- randomInt 2
    firstChoice <- randomInt 2
    hostOpens   <- pick $ NE.fromList [i | i <- [0, 1, 2], i /= firstChoice, i /= carIndex]
    let finalChoice = case s of
            Stay   -> firstChoice
            Change -> head [i | i <- [0, 1, 2], i /= firstChoice, i /= hostOpens]
    pure $ finalChoice == carIndex
