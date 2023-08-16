import           Control.Monad      (forever, void)
import           Data.Char          (toUpper)
import           Network.Simple.TCP (HostPreference (Host), acceptFork, listen)
import           Network.Socket     (socketToHandle)
import           System.IO          (BufferMode (LineBuffering), Handle,
                                     IOMode (ReadWriteMode), hGetLine,
                                     hPutStrLn, hSetBuffering)

main :: IO ()
main = listen (Host "127.0.0.1") "8765" $ \(socket, addr) -> do
    putStrLn $ "listening on: " ++ show addr
    forever $
        void $ acceptFork socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            h <- socketToHandle socket' ReadWriteMode
            handleClient h

handleClient :: Handle -> IO ()
handleClient h = do
    hSetBuffering h LineBuffering
    forever $ do
        line <- hGetLine h
        hPutStrLn h $ toUpper <$> line
