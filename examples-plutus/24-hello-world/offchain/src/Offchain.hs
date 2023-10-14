{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Offchain where

import           GeniusYield.GYConfig  (GYCoreConfig (cfgNetworkId),
                                        coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import qualified Gift

giftValidator :: GYValidator 'PlutusV2
giftValidator = validatorFromPlutus Gift.giftValidator

giftAddress :: GYTxQueryMonad m => m GYAddress
giftAddress = scriptAddress giftValidator

makeGift :: GYTxMonad m => GYValue -> m (GYTxSkeleton 'PlutusV2)
makeGift v = do
    addr <- giftAddress
    pure $ mustHaveOutput GYTxOut
        { gyTxOutAddress = addr
        , gyTxOutDatum   = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
        , gyTxOutValue   = v
        , gyTxOutRefS    = Nothing
        }

collectGift :: GYTxMonad m => GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
collectGift ref = do
    let datum    = datumFromPlutusData ()
        redeemer = redeemerFromPlutusData ()
    pure $ mustHaveInput GYTxIn
        { gyTxInTxOutRef = ref
        , gyTxInWitness = GYTxInWitnessScript (GYInScript giftValidator) datum redeemer
        }

makeGiftIO :: FilePath -> FilePath -> GYValue -> IO GYTxId
makeGiftIO cfgFile skeyFile v = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Gift" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (makeGift v)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

collectGiftIO :: FilePath -> FilePath -> GYTxOutRef -> IO GYTxId
collectGiftIO cfgFile skeyFile ref = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Gift" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (collectGift ref)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx
