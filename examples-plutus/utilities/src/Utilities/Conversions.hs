{-# LANGUAGE OverloadedStrings     #-}

module Utilities.Conversions
  (validatorHash') where

import           PlutusLedgerApi.V2       (BuiltinData, toBuiltin, ScriptHash (..))
import qualified PlutusLedgerApi.Common   as Plutus
import           PlutusTx                 (CompiledCode)
import qualified Cardano.Crypto.Hash      as Hash
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as SBS

-- https://github.com/input-output-hk/plutus/blob/056383e66ba8c611a3cfda1fc7afc8400a2c0641/plutus-benchmark/marlowe/src/PlutusBenchmark/Marlowe/Scripts/RolePayout.hs#L68
-- | Compute the hash of a script.
validatorHash' :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> ScriptHash
validatorHash' =
  -- FIXME: Apparently this is the wrong recipe, since its hash disagrees with `cardano-cli`.
  ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort)  -- For Plutus V2.
    . Plutus.serialiseCompiledCode
