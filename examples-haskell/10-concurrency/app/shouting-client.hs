import           Control.Concurrent (forkIO)
import           Control.Monad      (forever, void)
import           Network.Simple.TCP (connect)
import           Network.Socket     (socketToHandle)
import           System.IO          (Handle, IOMode (ReadWriteMode), hGetLine,
                                     hPutStrLn, stdin, stdout)

main :: IO ()
main = connect "127.0.0.1" "8765" $ \(socket, _) -> do
    h <- socketToHandle socket ReadWriteMode
    void $ forkIO $ copyByLine h stdout
    copyByLine stdin h

copyByLine :: Handle -> Handle -> IO ()
copyByLine from to = forever $ do
    line <- hGetLine from
    hPutStrLn to line
