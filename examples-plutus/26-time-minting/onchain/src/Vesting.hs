{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Vesting where

import           PlutusLedgerApi.V1.Interval (contains)
import           PlutusLedgerApi.V2          as PlutusV2
import           PlutusTx                    (CompiledCode, compile,
                                              unstableMakeIsData)
import           PlutusTx.Prelude            (Bool (..), elem, traceIfFalse,
                                              (&&))
import qualified Prelude
import           Utilities                   (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Vesting: Only unlocks at a specific time and only when signed by a specific PKH.

data VestingDatum = VestingDatum
    { vdDeadline    :: POSIXTime
    , vdBeneficiary :: PubKeyHash
    } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''VestingDatum -- Use TH to create an instance for IsData.

mkVestingValidator :: VestingDatum -> () -> PlutusV2.ScriptContext -> Bool
mkVestingValidator VestingDatum {..} () ctx =
    traceIfFalse "deadline not reached" deadlineReached &&
    traceIfFalse "beneficiary did not sign" beneficiarySigned
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    deadlineReached :: Bool
    deadlineReached = from vdDeadline `contains` txInfoValidRange info

    beneficiarySigned :: Bool
    beneficiarySigned = vdBeneficiary `elem` txInfoSignatories info

{-# INLINABLE mkVestingValidator #-}

vestingValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
vestingValidator = $$(PlutusTx.compile [|| wrappedVestingValidator ||])
  where
    wrappedVestingValidator = wrapValidator mkVestingValidator
