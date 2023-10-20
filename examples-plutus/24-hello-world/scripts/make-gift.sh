#!/bin/bash

unsigned=txs/make-gift.unsigned
signed=txs/make-gift.tx

cardano-cli address build \
    --payment-script-file assets/gift.plutus \
    --out-file assets/gift-preprod.addr

user=$(cat keys/user-preprod.addr)

cardano-cli transaction build \
    --change-address "$user" \
    --tx-in 9786e8dc85987b1cea7233a045e1321ca551c9a86b2220e1cb282257633a9fc7#1 \
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
