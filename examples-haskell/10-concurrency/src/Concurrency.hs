module Concurrency where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           Data.IORef

numberForever :: Int -> IO a
numberForever n = forever $ print n

second :: Int
second = 1000000

thread :: Int -> IO a
thread n = forever $ do
    print n
    threadDelay $ second `div` 10
