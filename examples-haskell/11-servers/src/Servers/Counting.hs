module Servers.Counting
    ( run
    ) where

import           Control.Concurrent          (forkFinally, forkIO)
import           Control.Concurrent.Async    (race)
import           Control.Concurrent.STM      (TChan, atomically, dupTChan,
                                              modifyTVar', newBroadcastTChanIO,
                                              newTVarIO, readTChan, readTVar,
                                              retry, writeTChan)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad               (forever, void)
import           Network.Simple.TCP          (HostPreference (Host), accept,
                                              listen)
import           Servers.Utils               (ReadLine, WriteLine, withSocket)

run :: IO ()
run = listen (Host "127.0.0.1") "8765" $ \(socket, addr) -> do
    putStrLn $ "listening on: " ++ show addr
    conns <- newTVarIO 0
    ch    <- newBroadcastTChanIO
    void $ forkIO $ monitor ch 0 conns
    forever $
        void $ accept socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            forkFinally
                (withSocket socket' $ handleClient conns ch)
                (const $ do
                    atomically $ modifyTVar' conns $
                        \x -> x - 1
                    putStrLn $ "client disconnected: " ++ show addr')

handleClient :: TVar Int
             -> TChan Int
             -> WriteLine
             -> ReadLine
             -> IO ()
handleClient conns ch writeLine readLine = do
    ch' <- atomically $ dupTChan ch
    atomically $ modifyTVar' conns (+ 1)
    void $ input `race` output ch'
  where
    input = forever readLine
    output ch' = forever $ do
        newCount <- atomically $ readTChan ch'
        writeLine $ "number of clients changed to: " ++ show newCount

monitor :: TChan Int -> Int -> TVar Int -> IO ()
monitor ch count conns = do
    newCount <- atomically $ do
        c <- readTVar conns
        if c == count
            then retry
            else do
                writeTChan ch c
                pure c
    monitor ch newCount conns
