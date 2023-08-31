{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Optics where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Map              (Map)
import qualified Data.Map              as Map

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

-- | Move all staff of a company to the given city.
goTo :: String -> Company -> Company
goTo c company = company {_staff = map f $ _staff company}
  where
    f :: Person -> Person
    f p = p {_address = g $ _address p}

    g :: Address -> Address
    g a = a {_city = c}

-- foreach (person in company.staff):
--    person.address.city = city

type Lens s a      = forall f. Functor f     => (a -> f a) -> (s -> f s)

type Traversal s a = forall f. Applicative f => (a -> f a) -> (s -> f s)

-- .   L T
--
-- L   L T
-- T   T T

-- Lens a x = forall f. Functor f => (x -> f x) -> (a -> f a)
-- Lens s a = forall f. Functor f =>               (a -> f a) -> (s -> f s)

over :: ((a -> Identity a) -> (s -> Identity s)) -> (a -> a) -> (s -> s)
over l f = runIdentity . l (Identity . f)

set :: ((a -> Identity a) -> (s -> Identity s)) -> s -> a -> s
set l s a = over l (const a) s

view :: ((a -> Const a a) -> (s -> Const a s)) -> s -> a
view l = getConst . l Const -- using overF at f = Const a

toListOf :: ((a -> Const [a] a) -> (s -> Const [a] s)) -> s -> [a]
toListOf l = getConst . l (Const . pure)

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens gt st f s = st s <$> f (gt s)

staff :: Lens Company [Person]
staff = lens _staff $ \c ps -> c {_staff = ps}

domain :: Lens Company String
domain = lens _domain $ \c d -> c {_domain = d}

name :: Lens Person String
name = lens _name $ \p n -> p {_name = n}

address :: Lens Person Address
address = lens _address $ \p a -> p {_address = a}

city :: Lens Address String
city = lens _city $ \a c -> a {_city = c}

country :: Lens Address String
country = lens _country $ \a c -> a {_country = c}

_1 :: Lens (a, b) a
_1 = lens fst $ \(_, b) a -> (a, b)

_2 :: Lens (a, b) b
_2 = lens snd $ \(a, _) b -> (a, b)

lazy :: Lens BS.ByteString LBS.ByteString
lazy = lens BS.fromStrict $ \_ l -> BS.toStrict l

strict :: Lens LBS.ByteString BS.ByteString
strict = lens BS.toStrict $ \_ l -> BS.fromStrict l

at :: forall k a. Ord k => k -> Lens (Map k a) (Maybe a)
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

-- | Move all staff of a company to the given city.
goTo' :: String -> Company -> Company
goTo' c company = company {_staff = map f $ _staff company}
  where
    f :: Person -> Person
    f p = set (address . city) p c

foo :: String -> IO String
foo s = do
    putStrLn $ "old name: " <> s
    getLine

each :: Traversable t => Traversal (t a) a
each = traverse

both :: Traversal (a, a) a
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
goTo'' :: String -> Company -> Company
goTo'' c company = set (staff.each.address.city) company c
