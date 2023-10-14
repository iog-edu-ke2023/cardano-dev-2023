#!/bin/bash

user=$(cat keys/user-preprod.addr)
unsigned=txs/collect-gift.unsigned
signed=txs/collect-gift.tx

cardano-cli transaction build \
    --babbage-era \
    --tx-in 9786e8dc85987b1cea7233a045e1321ca551c9a86b2220e1cb282257633a9fc7#0 \
    --tx-in-script-file "assets/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "assets/unit-datum.json" \
    --tx-in-collateral 9786e8dc85987b1cea7233a045e1321ca551c9a86b2220e1cb282257633a9fc7#1 \
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
