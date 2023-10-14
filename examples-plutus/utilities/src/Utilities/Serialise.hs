{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Utilities.Serialise
    ( writeValidatorToFile
    , dataToJSON
    , printDataToJSON
    , writeDataToFile
    ) where

import           Cardano.Api           (Error (displayError), prettyPrintJSON,
                                        unsafeHashableScriptData, writeFileJSON)
import           Cardano.Api.Shelley   (fromPlutusData,
                                        scriptDataToJsonDetailedSchema)
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as BS8
import           GeniusYield.Types     (PlutusVersion (PlutusV2),
                                        validatorFromPlutus, writeValidator)
import           PlutusLedgerApi.V1    (ToData)
import qualified PlutusLedgerApi.V2    as PlutusV2
import           PlutusTx              (BuiltinData, CompiledCode)
import           Text.Printf           (printf)

-- Create file with validator
writeValidatorToFile :: FilePath -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> IO ()
writeValidatorToFile filePath validator = writeValidator filePath (validatorFromPlutus @'PlutusV2 validator)

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
