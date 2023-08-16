{-# LANGUAGE NumericUnderscores #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           System.Random

type Account = TVar Integer

transfer :: Account -> Account -> Integer -> STM ()
transfer from to amount = do
    oldFrom <- readTVar from
    if oldFrom < amount
        then pure ()
        else do
            modifyTVar from (\x -> x - amount)
            modifyTVar to   (+ amount)

getTotal :: [Account] -> STM Integer
getTotal accounts = sum <$> mapM readTVar accounts

randomTransfer :: [Account] -> IO ()
randomTransfer accounts = do
    let maxIndex = length accounts - 1
    from   <- randomRIO (0, maxIndex)
    to     <- randomRIO (0, maxIndex)
    amount <- randomRIO (1, 100)
    atomically $ do
        transfer (accounts !! from) (accounts !! to) amount

monitor :: Integer -> [Account] -> IO a
monitor expected accounts = forever $ do
    actual <- atomically $ getTotal accounts
    when (actual /= expected) $
        putStrLn $
            "INVALID STATE: expected: " ++
            show expected               ++
            ", actual: "                ++
            show actual

main :: IO ()
main = do
    accounts <- mapM newTVarIO [1_000, 2_500]
    total    <- atomically $ getTotal accounts
    print total
    void $ forkIO $ monitor total accounts
    replicateConcurrently_ 100_000 $
        async (randomTransfer accounts)

transfer' :: Account
          -> Account
          -> Account
          -> Integer
          -> STM ()
transfer' from from' to amount =
    transfer from to amount `orElse`
    transfer from' to amount
