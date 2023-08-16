import           Concurrency        (second, thread)
import           Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = do
    mapM_ (forkIO . thread) [1 .. 10]
    threadDelay $ 5 * second
