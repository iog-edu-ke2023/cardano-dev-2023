{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module NegativeRTimed where

import           PlutusLedgerApi.V1.Interval (contains)
import           PlutusLedgerApi.V2          (POSIXTime,
                                              ScriptContext (scriptContextTxInfo),
                                              TxInfo (txInfoValidRange), from)
import           PlutusTx                    (CompiledCode, compile,
                                              unstableMakeIsData)
import           PlutusTx.Builtins           (BuiltinData, Integer)
import           PlutusTx.Prelude            (Bool, Ord ((<=)), traceIfFalse,
                                              ($), (&&))
import           Utilities                   (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

newtype CustomDatum = MkCustomDatum { deadline :: POSIXTime }
unstableMakeIsData ''CustomDatum

{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatum -> Integer -> ScriptContext -> Bool
mkValidator (MkCustomDatum d) r ctx = traceIfFalse "expected a negative redeemer" $ r <= 0 &&
                                      traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        deadlineReached :: Bool
        deadlineReached = contains (from d) $ txInfoValidRange info


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkWrappedValidator ||])
