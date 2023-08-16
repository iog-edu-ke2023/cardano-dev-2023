{-# LANGUAGE NumericUnderscores #-}

import           Control.Concurrent
import           Control.Monad
import           Data.IORef

thread :: IORef Int -> Int -> IO a
thread var n = forever $ do
    writeIORef var n
    x <- readIORef var
    when (x /= n) $
        print (x, n)

main :: IO ()
main = do
    var <- newIORef 0
    mapM_ (forkIO . thread var) [1 .. 10]
    threadDelay 5_000_000
