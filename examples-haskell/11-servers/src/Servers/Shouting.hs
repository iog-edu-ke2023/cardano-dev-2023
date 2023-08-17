module Servers.Shouting
    ( run
    ) where

import           Control.Concurrent (forkFinally)
import           Control.Monad      (forever, void)
import           Data.Char          (toUpper)
import           Network.Simple.TCP (HostPreference (Host), accept, listen)
import           Servers.Utils      (ReadLine, WriteLine, withSocket)

run :: IO ()
run = listen (Host "127.0.0.1") "8765" $ \(socket, addr) -> do
    putStrLn $ "listening on: " ++ show addr
    forever $
        void $ accept socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            forkFinally
                (withSocket socket' handleClient)
                (const $ putStrLn $ "client disconnected: " ++ show addr')

handleClient :: WriteLine -> ReadLine -> IO ()
handleClient writeLine readLine = do
    forever $ do
        line <- readLine
        writeLine $ toUpper <$> line
