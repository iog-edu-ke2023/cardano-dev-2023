module Merkle.Hash
    ( Hash
    , hash
    , hashToByteString
    ) where

import qualified Crypto.Hash          as Crypto
import qualified Data.ByteArray       as BA
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

type Hash = Crypto.Digest Crypto.SHA256

hash :: ByteString -> Hash
hash = Crypto.hashlazy

hashToByteString :: Hash -> ByteString
hashToByteString = LBS.pack . BA.unpack
