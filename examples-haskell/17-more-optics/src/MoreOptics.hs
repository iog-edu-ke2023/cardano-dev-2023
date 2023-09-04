{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MoreOptics where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Functor.Identity (Identity (..))
import           Data.Profunctor       (Choice (..), Profunctor (..))
import           Data.Tagged           (Tagged (..))
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Data.Monoid           (Sum (..))
import           Optics

type Prism s t a b = forall f p. (Applicative f, Choice p) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

{-
data Prism s a = Prism
    { preview :: s -> Maybe a
    , review  :: a -> s
    }
-}

prism :: forall s t a b. (s -> Either t a) -> (b -> t) -> Prism s t a b
prism p r x = dimap p (either pure $ fmap r) $ right' x

prism' :: (s -> Maybe a) -> (a -> s) -> Prism' s a
prism' p = prism p'
  where
    p' s = case p s of
        Nothing -> Left s
        Just a  -> Right a

re :: Iso s t a b -> Iso b a t s
re i = iso (review i) (view i)

-- idea: use f = Identity, p = Tagged
-- Tagged a (Identity a) -> Tagged s (Identity s)
-- Identity a -> Identity s
-- a -> s
review :: (Tagged a (Identity b) -> Tagged s (Identity t)) -> b -> t
review p = runIdentity . unTagged . p . Tagged . Identity


data Result a = Ok a | Error String
    deriving Show

_Ok :: Prism (Result a) (Result b) a b
_Ok = prism p r
  where
    p :: Result a -> Either (Result b) a
    p (Ok a)    = Right a
    p (Error e) = Left (Error e)

    r :: a -> Result a
    r = Ok

_Error :: Prism' (Result a) String
_Error = prism' p r
  where
    p :: Result a -> Maybe String
    p (Ok _)    = Nothing
    p (Error e) = Just e

    r :: String -> Result a
    r = Error

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism p Just
  where
    p :: Maybe a -> Either (Maybe b) a
    p (Just a) = Right a
    p Nothing  = Left Nothing

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' p r
  where
    p :: Maybe a -> Maybe ()
    p (Just _) = Nothing
    p Nothing  = Just ()

    r :: () -> Maybe a
    r () = Nothing

_Left :: forall a b a'. Prism (Either a b) (Either a' b) a a'
_Left = prism p Left
  where
    p :: Either a b -> Either (Either a' b) a
    p (Left a)  = Right a
    p (Right b) = Left (Right b)

_Right :: forall a b b'. Prism (Either a b) (Either a b') b b'
_Right = prism p Right
  where
    p :: Either a b -> Either (Either a b') b
    p (Left a)  = Left (Left a)
    p (Right b) = Right b

_Nil :: Prism' [a] ()
_Nil = prism' p r
  where
    p :: [a] -> Maybe ()
    p []      = Just ()
    p (_ : _) = Nothing

    r :: () -> [a]
    r () = []

_Cons :: Prism [a] [b] (a, [a]) (b, [b])
_Cons = prism p r
  where
    p :: [a] -> Either [b] (a, [a])
    p []       = Left []
    p (x : xs) = Right (x, xs)

    r :: (a, [a]) -> [a]
    r (x, xs) = x : xs

_Id :: Iso a b a b
_Id = iso id id

ex :: (Bool, [Maybe (Either Bool String)])
ex = (False, [Nothing, Just (Right ""), Just (Left True), Nothing, Just (Right "Haskell"), Just (Left False), Nothing, Just (Right "Kenya")])

maybeToEither :: forall a b. Iso (Maybe a) (Maybe b) (Either () a) (Either () b)
maybeToEither = iso v r
  where
    v :: Maybe a -> Either () a
    v Nothing  = Left ()
    v (Just a) = Right a

    r :: Either () b -> Maybe b
    r (Left ()) = Nothing
    r (Right b) = Just b

type Iso s t a b = forall f p. (Functor f, Choice p) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso v r = dimap v $ fmap r

reversed :: Iso [a] [b] [a] [b]
reversed = iso reverse reverse

curried :: Iso ((a, b) -> c) ((a', b') -> c') (a -> b -> c) (a' -> b' -> c')
curried = iso curry uncurry

flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip

swapped :: Iso (a, b) (a', b') (b, a) (b', a')
swapped = iso swap swap
  where
    swap (a, b) = (b, a)

swapped' :: Iso (Either a b) (Either a' b') (Either b a) (Either b' a')
swapped' = iso swap' swap'
  where
    swap' (Left a)  = Right a
    swap' (Right b) = Left b

summed :: Iso a b (Sum a) (Sum b)
summed = iso Sum getSum

-- over (each . re summed) (+ 3) [Sum 1, Sum 4] = [Sum 4, Sum 7]


lazy :: Iso' BS.ByteString LBS.ByteString
lazy = iso BS.fromStrict BS.toStrict

strict :: Iso' LBS.ByteString BS.ByteString
strict = re lazy

packed :: Iso' String Text
packed = iso T.pack T.unpack

unpacked :: Iso' Text String
unpacked = re packed
