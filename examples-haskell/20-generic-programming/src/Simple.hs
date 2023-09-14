{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Simple where

import           Control.Monad.State (MonadState (get, put), MonadTrans (lift),
                                      StateT (runStateT))
import           Data.Kind           (Type)

class Eq' a where
    eq :: a -> a -> Bool

data T = A | N T T
    deriving (Show, GEq)

instance Generic T where

    type Rep T = U :+: (T :*: T)

    from :: T -> Rep T
    from A       = L U
    from (N x y) = R (x :*: y)

    to :: Rep T -> T
    to (L U)         = A
    to (R (x :*: y)) = N x y

instance Generic (a, b) where
    type Rep (a, b) = a :*: b
    from (a, b) = a :*: b
    to (a :*: b) = (a, b)

instance (GEq a, GEq b) => GEq (a, b)

data Choice = I Int | C Char | B Choice Bool | S Choice
    deriving (Show, GEq)

instance Generic Choice where

    type Rep Choice = Int :+: (Char :+: ((Choice :*: Bool) :+: Choice))

    from :: Choice -> Rep Choice
    from (I n)   = L n
    from (C c)   = R $ L c
    from (B c b) = R $ R $ L $ c :*: b
    from (S c)   = R $ R $ R c

    to :: Rep Choice -> Choice
    to (L n)                 = I n
    to (R (L c))             = C c
    to (R (R (L (c :*: b)))) = B c b
    to (R (R (R c)))         = S c

instance GEq Bool

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, GEq, Ser)

data Rose a = Fork a [Rose a]
    deriving (Show, GEq, Ser)

instance Generic (Rose a) where

    type Rep (Rose a) = a :*: [Rose a]

    from :: Rose a -> Rep (Rose a)
    from (Fork a xs) = a :*: xs

    to :: Rep (Rose a) -> Rose a
    to (a :*: xs) = Fork a xs

instance GEq a => GEq [a]

data U = U -- ()
    deriving Show

data a :+: b = L a | R b -- Either a b
    deriving Show

data a :*: b = a :*: b -- (a, b)
    deriving Show

class Generic a where
    type Rep a :: Type
    from :: a -> Rep a
    to :: Rep a -> a

instance Generic Bool where

    type Rep Bool = U :+: U

    from :: Bool -> Rep Bool
    from False = L U
    from True  = R U

    to :: Rep Bool -> Bool
    to (L U) = False
    to (R U) = True

instance Generic [a] where

    type Rep [a] = U :+: (a :*: [a])

    from :: [a] -> Rep [a]
    from []       = L U
    from (x : xs) = R $ x :*: xs

    to :: Rep [a] -> [a]
    to (L U)          = []
    to (R (x :*: xs)) = x : xs

instance Generic (Tree a) where

    type Rep (Tree a) = a :+: (Tree a :*: Tree a)

    from :: Tree a -> Rep (Tree a)
    from (Leaf a)   = L a
    from (Node x y) = R (x :*: y)

    to :: Rep (Tree a) -> Tree a
    to (L a)         = Leaf a
    to (R (x :*: y)) = Node x y

data MyType a b =
      Flag Bool
    | Combo (a, a)
    | Other b Int (MyType a a)
    deriving (Show)

ex1 :: MyType String Bool
ex1 = Other False 42 $ Combo ("Haskell", "Java")

instance Generic (MyType a b) where

    type Rep (MyType a b) = Bool :+: ((a, a) :+: (b :*: (Int :*: MyType a a)))

    from :: MyType a b -> Rep (MyType a b)
    from (Flag b)      = L b
    from (Combo aa)    = R $ L aa
    from (Other b n x) = R $ R (b :*: (n :*: x))

    to :: Rep (MyType a b) -> MyType a b
    to (L b)                     = Flag b
    to (R (L aa))                = Combo aa
    to (R (R (b :*: (n :*: x)))) = Other b n x

class GEq a where
    geq :: a -> a -> Bool
    default geq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    geq = defaultEq

instance GEq U where
    geq U U = True

instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L a) (L a') = geq a a'
    geq (R b) (R b') = geq b b'
    geq _     _      = False

instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (a :*: b) (a' :*: b') = geq a a' && geq b b'

instance GEq Int where
    geq = (==)

instance GEq Char where
    geq = (==)

defaultEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
defaultEq x y = geq (from x) (from y)

data Bit = Zero | One
    deriving (Show, Eq, Ord)

newtype Parser a = Parser (StateT [Bit] [] a)
    deriving newtype (Functor, Applicative, Monad, MonadState [Bit])

runParser :: Parser a -> [Bit] -> [(a, [Bit])]
runParser (Parser x) = runStateT x

deserialize :: Ser a => [Bit] -> Maybe a
deserialize bs = case runParser deserialize' bs of
    [(a, [])] -> Just a
    _         -> Nothing

class Ser a where

    serialize :: a -> [Bit]
    default serialize :: (Generic a, Ser (Rep a)) => a -> [Bit]
    serialize = serialize . from

    deserialize' :: Parser a
    default deserialize' :: (Generic a, Ser (Rep a)) => Parser a
    deserialize' = to <$> deserialize'

instance Ser U where
    serialize U = []
    deserialize' = pure U

instance (Ser a, Ser b) => Ser (a :*: b) where
    serialize (a :*: b) = serialize a <> serialize b
    deserialize' = (:*:) <$> deserialize' <*> deserialize'

instance (Ser a, Ser b) => Ser (a :+: b) where
    serialize (L a) = Zero : serialize a
    serialize (R b) = One  : serialize b

    deserialize' = do
        bs <- get
        case bs of
            []          -> Parser $ lift []
            (Zero : cs) -> put cs >> L <$> deserialize'
            (One  : cs) -> put cs >> R <$> deserialize'

instance Ser a => Ser [a]
instance (Ser a, Ser b) => Ser (a, b)
instance Ser Bool

exTree :: Tree [Bool]
exTree = Node
    (Leaf [True, False])
    (Node (Leaf []) (Leaf [True]))

exRose :: Rose Bool
exRose = Fork True
    [ Fork True []
    , Fork False [Fork False []]
    , Fork True [Fork True [], Fork False []]
    ]

