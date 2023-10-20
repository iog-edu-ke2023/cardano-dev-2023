{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Atomic where

import           PlutusLedgerApi.V1.Value (geq)
import           PlutusLedgerApi.V2       as PlutusV2
import           PlutusTx                 (CompiledCode, compile,
                                           unstableMakeIsData)
import           PlutusTx.Prelude         (Bool (..), Eq ((==)), any, ($), (&&))
import qualified Prelude
import           Utilities                (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data AtomicDatum = AtomicDatum
    { adAddr  :: Address
    , adPrice :: Value
    } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''AtomicDatum -- Use TH to create an instance for IsData.

mkAtomicValidator :: AtomicDatum -> () -> PlutusV2.ScriptContext -> Bool
mkAtomicValidator AtomicDatum {..} () ScriptContext {..} =
    any p $ txInfoOutputs scriptContextTxInfo
  where
    p :: TxOut -> Bool
    p TxOut {..} = txOutAddress == adAddr && txOutValue `geq` adPrice
{-# INLINABLE mkAtomicValidator #-}

atomicValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
atomicValidator = $$(PlutusTx.compile [|| wrappedAtomicValidator ||])
  where
    wrappedAtomicValidator = wrapValidator mkAtomicValidator

-- DOUBLE SATISFACTION PROBLEM

-- Atomic: Seller, offers 50 H, price 1000 ADA
-- Atomic: Seller, offers 30 N, price  800 ADA
-- one TX, paying 1000 ADA to Seller
