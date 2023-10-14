#!/bin/bash

user=$(cat keys/user-preprod.addr)
unsigned=txs/make-gift.unsigned
signed=txs/make-gift.tx

cardano-cli address build \
    --payment-script-file assets/gift.plutus \
    --out-file assets/gift-preprod.addr

cardano-cli transaction build \
    --change-address "$user" \
    --tx-in 2b1e3eba74e6a6c64aeb02b617b366358a325f09b70381037df5f76479f8aac1#1 \
    --tx-out "$(cat assets/gift-preprod.addr) + 100000000 lovelace" \
    --tx-out-inline-datum-file "assets/unit-datum.json" \
    --out-file "$unsigned"

cardano-cli transaction sign \
    --tx-body-file "$unsigned" \
    --signing-key-file "keys/user.skey" \
    --out-file "$signed"

cardano-cli transaction submit \
    --tx-file "$signed"

cardano-cli transaction txid \
    --tx-file "$signed"
