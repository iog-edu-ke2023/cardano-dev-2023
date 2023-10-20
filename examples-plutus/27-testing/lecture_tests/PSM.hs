{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Prelude
import           Test.Tasty         (defaultMain, testGroup)

import           Control.Monad      (replicateM, when, unless)
import           Plutus.Model       (Ada (Lovelace), Run, ada, adaValue,
                                     defaultBabbage, mustFail, newUser,
                                     noErrors, sendValue, testNoErrors, valueAt, logError)
import           PlutusLedgerApi.V1 (PubKeyHash)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
      "Test simple user transactions"
      [ good "Simple spend" simpleSpend
      , bad  "Not enough funds" notEnoughFunds
      ]
      where
        bad msg = good msg . mustFail
        good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1_000)

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING TRANSACTIONS ----------------------------------------

-- Function to test that a simple transaction works
simpleSpend :: Run ()
simpleSpend = do
    users <- setupUsers                -- Create 3 users and assign each 1000 lovelaces
    let [u1, u2, u3] = users           -- Give names to individual users
    sendValue u1 (adaValue 100) u2     -- Send 100 lovelaces from user 1 to user 2
    sendValue u2 (adaValue 100) u3     -- Send 100 lovelaces from user 2 to user 3
    vals <- mapM valueAt users         -- Read user values
    unless (vals == fmap adaValue [900, 1_000, 1_100]) $
      logError "values don't match" 

-- Function to test that a transaction fails if there are not enough funds
notEnoughFunds :: Run ()
notEnoughFunds = do
  users <- setupUsers                -- Create 3 users and assign each 1000 lovelaces
  let [u1, u2, _u3] = users          -- Give names to individual users
  sendValue u1 (adaValue 10_000) u2  -- Send 10.000 lovelaces from user 1 to user 2
