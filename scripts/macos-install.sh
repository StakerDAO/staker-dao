#! /usr/bin/env bash
tezos_client_version="2020-03-11-ledger-patched"

brew tap gpevnev/stakerdao
brew install gpevnev/stakerdao/tezos-client-patched

tezos_client="$(brew --cellar gpevnev/stakerdao/tezos-client-patched)/$tezos_client_version/bin/tezos-client"

echo $tezos_client

brew install gpevnev/stakerdao/stkr-token-cli

config_dir="$HOME/.stkr-token-cli"
config_file="$config_dir/config.yaml"

mkdir -p config_dir

rm $config_file

touch $config_file
# TODO: figure out why writing to config file does not work
echo "envTezosClientCmd: \"$tezos_client\"" > $config_file
echo 'envNodeAddr:' >> $config_file
echo '    naHost: "carthage.testnet.tezos.serokell.team"' >> $config_file
echo '    naPort: 8732' >> $config_file
