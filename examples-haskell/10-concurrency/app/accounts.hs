{-# LANGUAGE NumericUnderscores #-}

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           System.Random

type Account = IORef Integer

transfer :: Account -> Account -> Integer -> IO ()
transfer from to amount = do
    atomicModifyIORef' from $ \oldFrom -> (oldFrom - amount, ())
    atomicModifyIORef' to   $ \oldTo   -> (oldTo   + amount, ())

getTotal :: [Account] -> IO Integer
getTotal accounts = sum <$> mapM readIORef accounts

randomTransfer :: [Account] -> IO ()
randomTransfer accounts = do
    let maxIndex = length accounts - 1
    from   <- randomRIO (0, maxIndex)
    to     <- randomRIO (0, maxIndex)
    amount <- randomRIO (1, 100)
    transfer (accounts !! from) (accounts !! to) amount

monitor :: Integer -> [Account] -> IO a
monitor expected accounts = forever $ do
    actual <- getTotal accounts
    when (actual /= expected) $
        putStrLn $
            "INVALID STATE: expected: " ++
            show expected               ++
            ", actual: "                ++
            show actual

main :: IO ()
main = do
    accounts <- mapM newIORef [1000, 2500]
    total    <- getTotal accounts
    print total
    void $ forkIO $ monitor total accounts
    replicateM_ 100_000 $
        forkIO (randomTransfer accounts)
    threadDelay 5_000_000


