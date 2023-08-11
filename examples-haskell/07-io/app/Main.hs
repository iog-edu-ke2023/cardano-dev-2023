import           IO                 (readFileLineByLine')
import           System.Environment (getArgs)

parseArgs :: IO FilePath
parseArgs = do
    xs <- getArgs
    case xs of
        [file] -> return file
        _      -> ioError $ userError "expected exactly one command line argument"

main :: IO ()
main = do
    file   <- parseArgs
    mlines <- readFileLineByLine' file
    print mlines

