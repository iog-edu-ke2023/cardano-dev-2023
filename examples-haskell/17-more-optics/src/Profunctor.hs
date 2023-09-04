{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}

module Profunctor
    ( Profunctor (..)
    , Choice (..)
    , Kleisli (..)
    , Tagged (..)
    ) where

import           Control.Category (Category (..))
import           Prelude          hiding (either, id, (.))

{-
instance Functor ((->) a) where
    fmap :: (b -> b') -> (->) a b -> (->) a b'
    fmap f g = f . g
-}
--                 g
--            a -------> b
--                             f
--                       b ----------> b'
--
--            a ---------------------> b'

--  a' ---->  a
--       h

class Profunctor f where
    dimap :: (a' -> a) -> (b -> b') -> f a b -> f a' b'

instance Profunctor (->) where
    dimap :: (a' -> a) -> (b -> b') -> (a -> b) -> a' -> b'
    dimap h f g = f . g . h

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance Monad m => Profunctor (Kleisli m) where
    dimap :: (a' -> a) -> (b -> b') -> Kleisli m a b -> Kleisli m a' b'
    dimap h f k = Kleisli $ fmap f . runKleisli k . h

--                   k
--             a -------> m b
--
--        h                         f
--  a' ------> a            b ------------> b'
--
--                                fmap f
--                        m b ----------> m b'
--
--  a' ---------------------------------> m b'

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
k >=> l = \a -> k a >>= l

-- monad laws are equivalent to:
-- return >=> a = a >=> return = a
-- (a >=> b) >=> c = a >=> (b >=> c)

instance Monad m => Category (Kleisli m) where
    id :: Kleisli m a a
    id = Kleisli pure

    -- a -> m a

    (.) :: Kleisli m b c -> Kleisli m a b -> Kleisli m a c
    k . l = Kleisli $ runKleisli l >=> runKleisli k -- \a -> runKleisli l a >>= runKleisli k

--                             b ------> m c
--                  a -----> m b


--                  a -----------------> m c


newtype Tagged a b = Tagged {unTagged :: b}

instance Profunctor Tagged where
    dimap :: (a' -> a) -> (b -> b') -> Tagged a b -> Tagged a' b'
    dimap _ f t = Tagged $ f $ unTagged t

class Profunctor p => Choice p where
    left'  :: p a b -> p (Either a c) (Either b c)
    left' pab = dimap (either Right Left) (either Right Left) $ right' pab

    right' :: p a b -> p (Either c a) (Either c b)
    right' pab = dimap (either Right Left) (either Right Left) $ left' pab

    {-# MINIMAL left' | right' #-}

either :: (a -> x) -> (b -> x) -> Either a b -> x
either ax _  (Left a)  = ax a
either _  bx (Right b) = bx b

instance Choice (->) where
    left' :: (a -> b) -> (Either a c -> Either b c)
    left' ab = either (Left . ab) Right

instance Monad m => Choice (Kleisli m) where
    left' :: Kleisli m a b -> Kleisli m (Either a c) (Either b c)
    left' k = Kleisli $ either (fmap Left . runKleisli k) (pure . Right)


--  a -------> m b

--  a          -----------> m (Either b c)

--  Either a c -----------> m (Either b c)

--  c          -----------> m (Either b c)
--  c    --> Either b c --> m (Either b c)
--      Right           pure

instance Choice Tagged where
    left' :: Tagged a b -> Tagged (Either a c) (Either b c)
    left' t = Tagged $ Left $ unTagged t
