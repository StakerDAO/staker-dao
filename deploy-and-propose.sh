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

duration=150
start=`date +%s`
start=$((start+duration-50))
timeConf=" --test --start $start --duration $duration"

function waitForStage {
  cur=`date +%s`
  stage=$1
  target=$((start+duration*stage+1))
  diff=$((target-cur))
  if [[ $diff -gt 0 ]]; then
    echo "Sleeping for ${diff}s ..."
    sleep "${diff}s"
  fi
}

msig=msig$I
stkr=stkr$I
from=zyoba
msigConf="--msigAlias $msig --stkrAlias $stkr --fromAlias $from --msigKeyAlias ${msig}_key_1 --msigKeyAlias ${msig}_key_2"

"$STKR_EXE" deploy $env --msigAlias $msig --stkrAlias $stkr --fromAlias $from $timeConf

waitForStage 0

"$STKR_EXE" new-proposal $env $msigConf --desc First --url "$some_url"

"$STKR_EXE" new-council $env $msigConf --prefix ${stkr}_council -n 3

waitForStage 2

"$STKR_EXE" vote $env --stkrAlias $stkr --fromAlias $from -p 1 -e 0 --memberKeyAlias ${stkr}_council_key_1
"$STKR_EXE" vote $env --stkrAlias $stkr --fromAlias $from -p 1 -e 0 --memberKeyAlias ${stkr}_council_key_2

"$STKR_EXE" print-storage $env --contractAlias $stkr
