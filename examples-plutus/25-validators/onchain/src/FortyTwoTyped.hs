{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module FortyTwoTyped where

import qualified PlutusLedgerApi.V2 as PlutusV2
import           PlutusTx           (BuiltinData, CompiledCode, compile)
import           PlutusTx.Prelude   (Bool, Eq ((==)), Integer, traceIfFalse,
                                     ($))
import           Prelude            (IO)
import           Utilities          (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This validator succeeds only if the redeemer is 42
--              Datum  Redeemer        ScriptContext
mk42Validator :: () -> Integer -> PlutusV2.ScriptContext -> Bool
mk42Validator _ r _ = traceIfFalse "expected 42" $ r == 42
{-# INLINABLE mk42Validator #-}

validator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| wrapValidator mk42Validator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/fortytwotyped.plutus" validator
