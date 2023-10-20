{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Burn where

import           PlutusTx         (BuiltinData, CompiledCode, compile)
import           PlutusTx.Prelude (traceError)
import           Prelude          (IO)
import           Utilities        (writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This validator always fails
--                    Datum         Redeemer     ScriptContext
mkBurnValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBurnValidator _ _ _ = traceError "it burns!!!"
{-# INLINABLE mkBurnValidator #-}

burnValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
burnValidator = $$(PlutusTx.compile [|| mkBurnValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/burn.plutus" burnValidator
