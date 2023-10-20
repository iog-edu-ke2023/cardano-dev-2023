{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Utilities.Serialise
    ( writePolicyToFile
    , writeValidatorToFile
    , dataToJSON
    , printDataToJSON
    , writeDataToFile
    , bytesToHex
    , writeCodeToFile
    , writeCodeToFile'
    ) where

import           Cardano.Api            (Error (displayError), PlutusScript,
                                         PlutusScriptV2, prettyPrintJSON,
                                         unsafeHashableScriptData, writeFileJSON)
import           Cardano.Api.Shelley    (PlutusScript (..), fromPlutusData,
                                         scriptDataToJsonDetailedSchema)
import           Codec.Serialise        (Serialise, serialise)
import           Data.Aeson             (Value)
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Short  as BSS
import           GeniusYield.Types      (PlutusVersion (PlutusV2),
                                         validatorFromPlutus, writeValidator
                                        , mintingPolicyFromPlutus, writeMintingPolicy
                                        , scriptFromPlutus, writeScript)
import           PlutusLedgerApi.V1     (ToData)
import qualified PlutusLedgerApi.V2     as PlutusV2
import           PlutusTx               (BuiltinData, CompiledCode)
import           Text.Printf            (printf)

-- Create file with validator
writeValidatorToFile :: FilePath -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> IO ()
writeValidatorToFile filePath validator = writeValidator filePath (validatorFromPlutus @'PlutusV2 validator)

writePolicyToFile :: FilePath -> CompiledCode (BuiltinData -> BuiltinData -> ()) -> IO ()
writePolicyToFile filePath policy =
 writeMintingPolicy filePath (mintingPolicyFromPlutus @'PlutusV2 policy)

writeCodeToFile :: FilePath -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) -> IO ()
writeCodeToFile filePath script =
  writeScript filePath (scriptFromPlutus @'PlutusV2 script)


writeCodeToFile' :: FilePath -> CompiledCode (BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) -> IO ()
writeCodeToFile' filePath script =
  writeScript filePath (scriptFromPlutus @'PlutusV2 script)

dataToJSON :: ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV2.toData

printDataToJSON :: ToData a => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

writeDataToFile :: ToData a => FilePath -> a -> IO ()
writeDataToFile filePath x = do
    let v = dataToJSON x
    writeFileJSON filePath v >>= \case
        Left err -> print $ displayError err
        Right () -> printf "Wrote data to: %s\n%s\n" filePath $ BS8.unpack $ prettyPrintJSON v

bytesToHex :: BS.ByteString -> BS.ByteString
bytesToHex = BS16.encode

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

