{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Minting where

import           PlutusCore.Version       (Version (..))
import           PlutusLedgerApi.V1.Value (flattenValue)
import           PlutusLedgerApi.V2       as PlutusV2
import           PlutusTx                 (CompiledCode, compile, liftCode,
                                           unsafeApplyCode, unstableMakeIsData)
import           PlutusTx.Prelude         (Bool (..), Eq ((==)), Integer, all,
                                           check, elem, encodeUtf8, error,
                                           otherwise, traceIfFalse, ($), (&&))
import qualified Prelude

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING POLICY -------------------------------------

-- Vesting: Only unlocks at a specific time and only when signed by a specific PKH. Only allows token name "Kenya".

data VestingDatum = VestingDatum
    { vdDeadline    :: POSIXTime
    , vdBeneficiary :: PubKeyHash
    } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''VestingDatum -- Use TH to create an instance for IsData.

-- Minting and burning should only be allowed if a specific party sign the transaction.
mkTypedMintingPolicy :: PubKeyHash -> () -> PlutusV2.ScriptContext -> Bool
mkTypedMintingPolicy pkh () ctx =
    traceIfFalse "signature missing" hasSignature &&
    traceIfFalse "wrong token name" correctTokenName

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = TokenName $ encodeUtf8 "Kenya"

    hasSignature :: Bool
    hasSignature = pkh `elem` txInfoSignatories info

    correctTokenName :: Bool
    correctTokenName = all f $ flattenValue $ txInfoMint info
      where
        f :: (CurrencySymbol, TokenName, Integer) -> Bool
        f (cs', tn', _)
            | cs' == cs = tn' == tn
            | otherwise = True

    cs :: CurrencySymbol
    cs = case scriptContextPurpose ctx of
            Minting cs' -> cs'
            _           -> error ()

{-# INLINABLE mkTypedMintingPolicy #-}

mkMintingPolicy :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinData -> ())
mkMintingPolicy pkh = $$(PlutusTx.compile [|| wrappedMintingPolicy ||]) `unsafeApplyCode`
    liftCode (Version 1 0 0) pkh
  where
    wrappedMintingPolicy :: PubKeyHash -> BuiltinData -> BuiltinData -> ()
    wrappedMintingPolicy pkh' r ctx = check $
        mkTypedMintingPolicy pkh' (unsafeFromBuiltinData r) (unsafeFromBuiltinData ctx)
