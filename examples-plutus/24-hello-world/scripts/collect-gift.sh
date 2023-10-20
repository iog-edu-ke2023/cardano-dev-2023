#!/bin/bash

user=$(cat keys/user-preprod.addr)
unsigned=txs/collect-gift.unsigned
signed=txs/collect-gift.tx

cardano-cli transaction build \
    --babbage-era \
    --tx-in 96b832a049a22e89e31655b49c7a17e1fe5f8894b68d3f0f1184e40aeaafcb4e#0 \
    --tx-in-script-file "assets/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "assets/unit-datum.json" \
    --tx-in-collateral 96b832a049a22e89e31655b49c7a17e1fe5f8894b68d3f0f1184e40aeaafcb4e#1 \
    --change-address "$user" \
    --out-file "$unsigned"

cardano-cli transaction sign \
    --tx-body-file "$unsigned" \
    --signing-key-file "keys/user.skey" \
    --out-file "$signed"

cardano-cli transaction submit \
    --tx-file "$signed"

cardano-cli transaction txid \
    --tx-file "$signed"
