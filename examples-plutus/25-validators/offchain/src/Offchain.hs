{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Offchain where

import           Data.Map.Strict       (Map)
import           GeniusYield.GYConfig  (GYCoreConfig (cfgNetworkId),
                                        coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusTx              (BuiltinData)

import qualified Atomic
import qualified Burn
import qualified FortyTwo

burnValidator :: GYValidator 'PlutusV2
burnValidator = validatorFromPlutus Burn.burnValidator

burnAddress :: GYTxQueryMonad m => m GYAddress
burnAddress = scriptAddress burnValidator

fortyTwoValidator :: GYValidator 'PlutusV2
fortyTwoValidator = validatorFromPlutus FortyTwo.validator

fortyTwoAddress :: GYTxQueryMonad m => m GYAddress
fortyTwoAddress = scriptAddress fortyTwoValidator

atomicValidator :: GYValidator 'PlutusV2
atomicValidator = validatorFromPlutus Atomic.atomicValidator

atomicAddress :: GYTxQueryMonad m => m GYAddress
atomicAddress = scriptAddress atomicValidator

makeGift :: GYTxMonad m => GYValue -> m (GYTxSkeleton 'PlutusV2)
makeGift v = do
    addr <- burnAddress
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
        , gyTxInWitness = GYTxInWitnessScript (GYInScript burnValidator) datum redeemer
        }

makeGiftIO :: FilePath -> FilePath -> GYValue -> IO GYTxId
makeGiftIO cfgFile skeyFile v = do
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
    withCfgProviders cfg "Burn" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (collectGift ref)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

mk42 :: GYTxMonad m => GYValue -> m (GYTxSkeleton 'PlutusV2)
mk42 v = do
    addr <- fortyTwoAddress
    pure $ mustHaveOutput GYTxOut
        { gyTxOutAddress = addr
        , gyTxOutDatum   = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
        , gyTxOutValue   = v
        , gyTxOutRefS    = Nothing
        }

collect42 :: GYTxMonad m => GYTxOutRef -> BuiltinData -> m (GYTxSkeleton 'PlutusV2)
collect42 ref r = do
    let datum    = datumFromPlutusData ()
        redeemer = redeemerFromPlutus' r
    pure $ mustHaveInput GYTxIn
        { gyTxInTxOutRef = ref
        , gyTxInWitness = GYTxInWitnessScript (GYInScript fortyTwoValidator) datum redeemer
        }

mk42IO :: FilePath -> FilePath -> GYValue -> IO GYTxId
mk42IO cfgFile skeyFile v = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "42" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (mk42 v)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

collect42IO :: FilePath -> FilePath -> GYTxOutRef -> BuiltinData -> IO GYTxId
collect42IO cfgFile skeyFile ref r = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "42" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (collect42 ref r)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

placeAtomic :: GYTxMonad m => GYValue -> GYValue -> m (GYTxSkeleton 'PlutusV2)
placeAtomic toSell price = do
    contractAddr <- atomicAddress
    ownAddress'  <- head <$> ownAddresses
    let d = Atomic.AtomicDatum
                { Atomic.adAddr  = addressToPlutus ownAddress'
                , Atomic.adPrice = valueToPlutus price
                }
    pure $ mustHaveOutput GYTxOut
        { gyTxOutAddress = contractAddr
        , gyTxOutDatum   = Just (datumFromPlutusData d, GYTxOutUseInlineDatum)
        , gyTxOutValue   = toSell
        , gyTxOutRefS    = Nothing
        }

scanAtomic :: GYTxQueryMonad m => m (Map GYTxOutRef (GYValue, Atomic.AtomicDatum))
scanAtomic = do
    contractAddr <- atomicAddress
    utxos        <- utxosAtAddress contractAddr
    witherUTxOs atomicDatum utxos

atomicDatum :: GYTxQueryMonad m => GYUTxO -> m (Maybe (GYValue, Atomic.AtomicDatum))
atomicDatum utxo = do
    addr <- atomicAddress
    e    <- utxoDatum utxo
    pure $ case e of
        Left _          -> Nothing
        Right (a, v, d)
            | a == addr -> Just (v, d)
            | otherwise -> Nothing

buyAtomic :: GYTxMonad m => GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
buyAtomic ref = do
    utxo <- utxoAtTxOutRef' ref
    m <- atomicDatum utxo
    case m of
        Nothing     -> error "invalid ref"
        Just (_, d) -> do
            price  <- valueFromPlutus' $ Atomic.adPrice d
            seller <- addressFromPlutus' $ Atomic.adAddr d
            let datum = datumFromPlutusData d
                redeemer = redeemerFromPlutusData ()
            pure $
                mustHaveInput GYTxIn
                    { gyTxInTxOutRef = ref
                    , gyTxInWitness = GYTxInWitnessScript (GYInScript atomicValidator) datum redeemer
                    } <>
                mustHaveOutput GYTxOut
                    { gyTxOutAddress = seller
                    , gyTxOutDatum   = Nothing
                    , gyTxOutValue   = price
                    , gyTxOutRefS    = Nothing
                    }

placeAtomicIO :: FilePath -> FilePath -> GYValue -> GYValue -> IO GYTxId
placeAtomicIO cfgFile skeyFile toSell price = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "atomic" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (placeAtomic toSell price)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

scanAtomicIO :: FilePath -> IO (Map GYTxOutRef (GYValue, Atomic.AtomicDatum))
scanAtomicIO cfgFile = do
    cfg  <- coreConfigIO cfgFile
    let nid  = cfgNetworkId cfg
    withCfgProviders cfg "atomic" $ \providers ->
        runGYTxQueryMonadNode nid providers scanAtomic

buyAtomicIO :: FilePath -> FilePath -> GYTxOutRef -> IO GYTxId
buyAtomicIO cfgFile skeyFile ref = do
    cfg  <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "atomic" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (buyAtomic ref)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx
