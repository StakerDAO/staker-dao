#!/usr/bin/env bash

if [ "$I" == "" ] || [ "$STKR_EXE" == "" ]; then
  echo "Usage: STKR_EXE=./result/bin/stkr-token-exe I=5 $0"
  exit 1
fi

if [[ "$TEZOS_CLIENT" == "" ]]; then
  TEZOS_CLIENT=./tezos-client
fi

env="--tzclient $TEZOS_CLIENT -A jupiter.serokell.io -P 8732 "

some_url=tezos-wp:be7663e0ef87d51ab149a60dfad4df5940d30395ba287d9907f8d66ce5061d96:https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf

start=`date +%s`
timeConf=" --test --start $start --duration 600"

msig=msig$I
stkr=stkr$I
from=zyoba

# "$STKR_EXE" deploy $env --msigAlias $msig --stkrAlias $stkr --fromAlias $from $timeConf
# 
"$STKR_EXE" new-proposal $env --msigAlias $msig --stkrAlias $stkr --fromAlias $from \
    --msigKeyAlias ${msig}_key_1 --msigKeyAlias ${msig}_key_2 \
    --desc First --url "$some_url"

"$STKR_EXE" print-storage $env --contractAlias $stkr
