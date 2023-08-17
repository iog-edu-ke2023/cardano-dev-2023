module List where

--   SEND
-- + MORE
-- ------
--  MONEY

draw :: [a] -> [(a, [a])]
draw []       = []
draw (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- draw xs]

check :: Bool -> [()]
check True  = [()]
check False = []

data Solution = Solution
    { solS :: Int
    , solE :: Int
    , solN :: Int
    , solD :: Int
    , solM :: Int
    , solO :: Int
    , solR :: Int
    , solY :: Int
    } deriving Show

sendMoreMoney :: [Solution]
sendMoreMoney = do
    (d, xs1) <- draw [0 .. 9]
    (e, xs2) <- draw xs1
    (y, xs3) <- draw xs2

    check $ y == (d + e) `mod` 10
    let c1 = (d + e) `div` 10

    (n, xs4) <- draw xs3
    (r, xs5) <- draw xs4

    check $ e == (n + r + c1) `mod` 10
    let c2 = (n + r + c1) `div` 10

    (o, xs6) <- draw xs5

    check $ n == (e + o + c2) `mod` 10
    let c3 = (e + o + c2) `div` 10

    (s, xs7) <- draw xs6
    check $ s /= 0

    (m, _) <- draw xs7
    check $ m /= 0
    check $ s + m + c3 == 10 * m + o

    pure Solution
        { solS = s
        , solE = e
        , solN = n
        , solD = d
        , solM = m
        , solO = o
        , solR = r
        , solY = y
        }

