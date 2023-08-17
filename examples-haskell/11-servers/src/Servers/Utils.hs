module Servers.Utils
    ( WriteLine
    , ReadLine
    , withSocket
    ) where

import           Network.Simple.TCP (Socket)
import           Network.Socket     (socketToHandle)
import           System.IO          (BufferMode (LineBuffering),
                                     IOMode (ReadWriteMode), hGetLine,
                                     hPutStrLn, hSetBuffering)
import           System.IO.Error    (catchIOError)

type WriteLine = String -> IO ()
type ReadLine = IO String

withSocket :: Socket
           -> (WriteLine -> ReadLine -> IO ())
           -> IO ()
withSocket socket handler = do
    h <- socketToHandle socket ReadWriteMode
    hSetBuffering h LineBuffering
    handler (hPutStrLn h) (hGetLine h)
        `catchIOError`
            const (pure ())
