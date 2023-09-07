import           ReaderIO
import           System.Environment (getArgs)

main :: IO ()
main = do
    [logFile] <- getArgs
    runM exampleProg logFile
