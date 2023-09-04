{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Optics where

import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           (First (..))

data Company = Company
    { _staff  :: [Person]
    , _domain :: String
    } deriving Show

data Person = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

data Address = Address
    { _country :: String
    , _city    :: String
    } deriving Show

karina, lars :: Person
karina = Person
    { _name    = "Karina"
    , _address = Address
        { _city    = "Zacatecas"
        , _country = "Mexico"
        }
    }
lars = Person
    { _name    = "Lars"
    , _address = Address
        { _city    = "Regensburg"
        , _country = "Germany"
        }
    }

iog :: Company
iog = Company [lars, karina] "blockchain"

type Lens s t a b      = forall f. Functor f     => (a -> f b) -> (s -> f t)
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Lens' s a = Lens s s a a
type Traversal' s a = Traversal s s a a

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> (t a -> f (t b))

-- .   L T
--
-- L   L T
-- T   T T

-- Lens a x = forall f. Functor f => (x -> f x) -> (a -> f a)
-- Lens s a = forall f. Functor f =>               (a -> f a) -> (s -> f s)

over :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> (s -> t)
over l f = runIdentity . l (Identity . f)

set :: ((a -> Identity b) -> (s -> Identity t)) -> s -> b -> t
set l s a = over l (const a) s

view :: ((a -> Const a b) -> (s -> Const a t)) -> s -> a
view l = getConst . l Const -- using overF at f = Const a

toListOf :: ((a -> Const [a] b) -> (s -> Const [a] t)) -> s -> [a]
toListOf l = getConst . l (Const . pure)

preview :: ((a -> Const (First a) b) -> (s -> Const (First a) t)) -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens gt st f s = st s <$> f (gt s)

staff :: Lens' Company [Person]
staff = lens _staff $ \c ps -> c {_staff = ps}

domain :: Lens' Company String
domain = lens _domain $ \c d -> c {_domain = d}

name :: Lens' Person String
name = lens _name $ \p n -> p {_name = n}

address :: Lens' Person Address
address = lens _address $ \p a -> p {_address = a}

city :: Lens' Address String
city = lens _city $ \a c -> a {_city = c}

country :: Lens' Address String
country = lens _country $ \a c -> a {_country = c}

_1 :: Lens (a, b) (a', b) a a'
_1 = lens fst $ \(_, b) a -> (a, b)

_2 :: Lens (a, b) (a, b') b b'
_2 = lens snd $ \(a, _) b -> (a, b)

at :: forall k a. Ord k => k -> Lens' (Map k a) (Maybe a)
at k = lens gt st
  where
    gt :: Map k a -> Maybe a
    gt = Map.lookup k

    st :: Map k a -> Maybe a -> Map k a
    st m (Just a) = Map.insert k a m
    st m Nothing  = Map.delete k m

-- over ax           :: (x -> x) -> (a -> a)
-- over sa           ::             (a -> a) -> (s -> s)
-- over sa . over ax :: (x -> x)       ->       (s -> s)

foo :: String -> IO String
foo s = do
    putStrLn $ "old name: " <> s
    getLine

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

both :: Traversal (a, a) (b, b) a b
both f (a, a') = (,) <$> f a <*> f a'

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable, Traversable)

tree :: Tree String
tree = Node
    (Leaf "Haskell")
    (Node
        (Leaf "Python")
        (Leaf "Java"))

-- | Move all staff of a company to the given city.
goTo :: String -> Company -> Company
goTo c company = set (staff.each.address.city) company c
