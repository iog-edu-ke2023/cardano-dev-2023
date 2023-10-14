{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gift where

import           PlutusTx  (BuiltinData, CompiledCode, compile)
import           Prelude   (IO)
import           Utilities (writeDataToFile, writeValidatorToFile)

mkGiftValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGiftValidator _datum _redeemer _ctx = ()
{-# INLINABLE mkGiftValidator #-}

giftValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
giftValidator = $$(PlutusTx.compile [|| mkGiftValidator ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "../assets/gift.plutus" giftValidator

saveDatum :: IO ()
saveDatum = writeDataToFile "../assets/unit-datum.json" ()
