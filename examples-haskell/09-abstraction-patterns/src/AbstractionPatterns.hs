{-# LANGUAGE RecordWildCards #-}

module AbstractionPatterns where

import           Control.Monad.State hiding (ap, modify, unless, when)
import           Numeric.Natural     (Natural)
import qualified Table               as T
import           Table               (Table)

type Bank = Table String Natural

data Tx = Tx
    { txFrom :: String
    , txTo   :: String
    , txAmt  :: Natural
    } deriving (Show)

processTx :: Tx -> Bank -> Maybe Bank
processTx Tx {..} bank
    | txFrom == txTo || txAmt == 0 = Nothing
    | otherwise                    =
        case T.lookup txFrom bank of
            Nothing      -> Nothing
            Just oldFrom
                | oldFrom < txAmt -> Nothing
                | otherwise          ->
                    case T.lookup txTo bank of
                        Nothing    -> Nothing
                        Just oldTo ->
                          let
                            newFrom = oldFrom - txAmt
                            newTo   = oldTo + txAmt
                          in
                            Just $ T.insert txFrom newFrom $
                                   T.insert txTo newTo bank

initialBank :: Bank
initialBank = T.insert "Robertino" 100 $
       T.insert "Karina" 250 T.empty

tx1, tx2 :: Tx
tx1 = Tx {txFrom = "Robertino", txTo = "Karina", txAmt = 150}
tx2 = Tx {txFrom = "Karina", txTo = "Robertino", txAmt = 70}

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
{-
bindMaybe m f = case m of
    Nothing -> Nothing
    Just x  -> f x
-}
bindMaybe m f = maybe Nothing f m

checkMaybe :: Bool -> Maybe a -> Maybe a
checkMaybe b m
    | b         = Nothing
    | otherwise = m

processTx' :: Tx -> Bank -> Maybe Bank
processTx' Tx {..} bank =
    checkMaybe (txFrom == txTo || txAmt == 0) $
        T.lookup txFrom bank `bindMaybe` \oldFrom ->
            checkMaybe (oldFrom < txAmt) $
                T.lookup txTo bank `bindMaybe` \oldTo ->
                  let
                    newFrom = oldFrom - txAmt
                    newTo   = oldTo + txAmt
                  in
                    Just $ T.insert txFrom newFrom $
                           T.insert txTo newTo bank

throwError :: String -> Either String a
throwError = Left

processTxE :: Tx -> Bank -> Either String Bank
processTxE Tx {..} bank
    | txFrom == txTo = throwError "sender equals receiver"
    | txAmt == 0     = throwError "zero amount"
    | otherwise      =
        case T.lookup txFrom bank of
            Nothing      -> throwError "unknown sender"
            Just oldFrom
                | oldFrom < txAmt ->
                    throwError $
                        "insufficient funds, missing " ++
                        show (txAmt - oldFrom)
                | otherwise          ->
                    case T.lookup txTo bank of
                        Nothing    -> throwError "unknown receiver"
                        Just oldTo ->
                          let
                            newFrom = oldFrom - txAmt
                            newTo   = oldTo + txAmt
                          in
                            Right $ T.insert txFrom newFrom $
                                    T.insert txTo newTo bank

bindEither :: Either e a -> (a -> Either e b) -> Either e b
bindEither x f = case x of
    Left err -> Left err
    Right a  -> f a

lookupE :: String -> String -> Bank -> Either String Natural
lookupE msg account bank = case T.lookup account bank of
    Nothing  -> throwError msg
    Just amt -> Right amt

processTxE' :: Tx -> Bank -> Either String Bank
processTxE' Tx {..} bank
    | txFrom == txTo = throwError "sender equals receiver"
    | txAmt == 0     = throwError "zero amount"
    | otherwise      =
        lookupE "unknown sender" txFrom bank `bindEither` \oldFrom ->
            if oldFrom < txAmt
                then throwError $
                    "insufficient funds, missing " ++
                    show (txAmt - oldFrom)
                else lookupE
                    "unknown receiver" txTo bank `bindEither` \oldTo ->
                  let
                    newFrom = oldFrom - txAmt
                    newTo   = oldTo + txAmt
                  in
                    Right $ T.insert txFrom newFrom $
                            T.insert txTo newTo bank

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

label :: Tree a -> Tree (Int, a)
label t = snd $ label' t 1

label' :: Tree a -> Int -> (Int, Tree (Int, a))
label' (Leaf a)   nextLabel = (nextLabel + 1, Leaf (nextLabel, a))
label' (Node l r) nextLabel =
  let
    (newNextLabel, l')       = label' l nextLabel
    (evenNewerNextLabel, r') = label' r newNextLabel
  in
    (evenNewerNextLabel, Node l' r')

tree :: Tree Char
tree = Node (Leaf 'G') $ Node (Leaf 'X') (Leaf 'Q')

{-
newtype State s a = State {runState :: s -> (s, a)}

bindState :: State s a -> (a -> State s b) -> State s b
bindState s k = State $ \oldState ->
  let
    (updatedState, a) = runState s oldState
  in
    runState (k a) updatedState

get :: State s s
get = State $ \currentState -> (currentState, currentState)

put :: s -> State s ()
put newState = State $ const (newState, ())

pureS :: a -> State s a
pureS a = State $ \s -> (s, a)

-}

labelS :: Tree a -> State Int (Tree (Int, a))
labelS (Leaf a)   =
    get                 >>= \nextLabel ->
    put (nextLabel + 1) >>= \()        ->
    pure $ Leaf (nextLabel, a)
labelS (Node l r) =
    labelS l >>= \l' ->
    labelS r >>= \r' ->
    pure $ Node l' r'

bindList :: [a] -> (a -> [b]) -> [b]
bindList = flip concatMap

-- Given a list of lines of text, list all the word lengths of
-- every word in every line.
wordLengths :: [String] -> [Int]
wordLengths lines' =
    lines'     `bindList` \line ->
    words line `bindList` \word ->
    [length word]

-- bindMaybe  :: Maybe a    -> (a -> Maybe b)    -> Maybe b
-- bindEither :: Either e a -> (a -> Either e b) -> Either e b
-- bindState  :: State s a  -> (a -> State s b)  -> State s b
-- bindList   :: [a]        -> (a -> [b])        -> [b]
-- (>>=)      :: IO a       -> (a -> IO b)       -> IO b

{-
class Functor m => Applicative m where
    pure  :: a -> m a
    (<*>) :: m (a -> b) -> m a -> m b

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
-}

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
    f <- mf
    a <- ma
    pure $ f a

when, unless :: Applicative m => Bool -> m () -> m ()
when True  action = action
when False _      = pure ()

unless b = when (not b)

processTxM :: Tx -> Bank -> Maybe Bank
processTxM Tx {..} bank = do
    when (txFrom == txTo || txAmt == 0)
        Nothing
    oldFrom <- T.lookup txFrom bank
    when (oldFrom < txAmt)
        Nothing
    oldTo <- T.lookup txTo bank
    let newFrom = oldFrom - txAmt
        newTo   = oldTo + txAmt
    pure $ T.insert txFrom newFrom $
           T.insert txTo newTo bank

processTxEM :: Tx -> Bank -> Either String Bank
processTxEM Tx {..} bank = do
    when (txFrom == txTo) $
        throwError "sender equals receiver"
    when (txAmt == 0) $
        throwError "zero amount"
    oldFrom <- lookupE "unknown sender" txFrom bank
    when (oldFrom < txAmt) $
        throwError $
            "insufficient funds, missing " ++
            show (txAmt - oldFrom)
    oldTo <- lookupE "unknown receiver" txTo bank
    let newFrom = oldFrom - txAmt
        newTo   = oldTo + txAmt
    pure $ T.insert txFrom newFrom $
           T.insert txTo newTo bank

labelSM :: Tree a -> State Int (Tree (Int, a))
labelSM (Leaf a)   = do
    nextLabel <- get
    put (nextLabel + 1)
    pure $ Leaf (nextLabel, a)
labelSM (Node l r) = do
    l' <- labelS l
    r' <- labelS r
    pure $ Node l' r'

labelSA :: Tree a -> State Int (Tree (Int, a))
labelSA (Leaf a)   = do
    nextLabel <- get
    put (nextLabel + 1)
    pure $ Leaf (nextLabel, a)
labelSA (Node l r) = Node <$> labelS l <*> labelS r

-- Given a list of lines of text, list all the word lengths of
-- every word in every line.
wordLengths' :: [String] -> [Int]
wordLengths' lines' = do
    line <- lines'
    word <- words line
    pure $ length word

wordLengths'' :: [String] -> [Int]
wordLengths'' lines' =
    [ length word
    | line <- lines'
    , word <- words line
    ]

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader r r
ask = Reader id

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = Reader . const
    (<*>) = ap

instance Monad (Reader r) where
    ma >>= k = Reader $ \s ->
      let
        a = runReader ma s
      in
        runReader (k a) s

data Rose a = Fork a [Rose a]
    deriving Show

rose :: Rose Bool
rose  =
    Fork False
        [ Fork True []
        , Fork False [Fork True [], Fork True []]
        , Fork False []
        ]

labelRose :: Rose a -> Rose (Int, a)
labelRose t = evalState (labelRoseS t) 1

labelRoseS :: Rose a -> State Int (Rose (Int, a))
labelRoseS (Fork a xs) = do
    l <- get
    put $ l + 1
    let a' = (l, a)
    xs' <- mapM labelRoseS xs
    pure $ Fork a' xs'

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put $ f s
