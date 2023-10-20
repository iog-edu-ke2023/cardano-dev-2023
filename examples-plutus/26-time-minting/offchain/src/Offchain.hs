{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Offchain where

import           GeniusYield.GYConfig  (GYCoreConfig (cfgNetworkId),
                                        coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types

import qualified Minting
import qualified Vesting
import           Vesting               (VestingDatum (VestingDatum))

vestingValidator :: GYValidator 'PlutusV2
vestingValidator = validatorFromPlutus Vesting.vestingValidator

vestingAddress :: GYTxQueryMonad m => m GYAddress
vestingAddress = scriptAddress vestingValidator

placeVesting :: GYTxMonad m => GYValue -> GYTime -> GYPubKeyHash -> m (GYTxSkeleton 'PlutusV2)
placeVesting v deadline beneficiary = do
    addr <- vestingAddress
    let d = Vesting.VestingDatum
                { Vesting.vdDeadline = timeToPlutus deadline
                , Vesting.vdBeneficiary = pubKeyHashToPlutus beneficiary
                }
    pure $ mustHaveOutput GYTxOut
        { gyTxOutAddress = addr
        , gyTxOutDatum   = Just (datumFromPlutusData d, GYTxOutUseInlineDatum)
        , gyTxOutValue   = v
        , gyTxOutRefS    = Nothing
        }

unlockVesting :: GYTxMonad m => GYTxOutRef -> GYTime -> m (GYTxSkeleton 'PlutusV2)
unlockVesting ref time = do
    utxo <- utxoAtTxOutRef' ref
    (_, _, d) <- utxoDatum' utxo
    pkh       <- pubKeyHashFromPlutus' $ Vesting.vdBeneficiary d
    slot      <- enclosingSlotFromTime' time
    let datum    = datumFromPlutusData @Vesting.VestingDatum d
        redeemer = redeemerFromPlutusData ()
    pure $
        mustHaveInput GYTxIn
            { gyTxInTxOutRef = ref
            , gyTxInWitness = GYTxInWitnessScript (GYInScript vestingValidator) datum redeemer
            }              <>
        mustBeSignedBy pkh <>
        isInvalidBefore slot

placeVestingIO :: FilePath -> FilePath -> GYValue -> GYTime -> GYPubKeyHash -> IO GYTxId
placeVestingIO cfgFile skeyFile v deadline beneficiary = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Burn" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (placeVesting v deadline beneficiary)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

unlockVestingIO :: FilePath -> FilePath -> GYTxOutRef -> IO GYTxId
unlockVestingIO cfgFile skeyFile ref = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    now  <- getCurrentGYTime
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Burn" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (unlockVesting ref now)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

mkMintingPolicy :: GYPubKeyHash -> GYMintingPolicy 'PlutusV2
mkMintingPolicy pkh = mintingPolicyFromPlutus $ Minting.mkMintingPolicy $ pubKeyHashToPlutus pkh

mint :: GYTxMonad m => Integer -> m (GYTxSkeleton 'PlutusV2)
mint amount = do
    addr <- head <$> ownAddresses
    pkh  <- addressToPubKeyHash' addr
    pure $
        mustMint (GYMintScript $ mkMintingPolicy pkh) (redeemerFromPlutusData ()) "Kenya" amount <>
        mustBeSignedBy pkh

mintIO :: FilePath -> FilePath -> Integer -> IO GYTxId
mintIO cfgFile skeyFile amount = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Burn" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (mint amount)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx
