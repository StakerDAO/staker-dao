#! /usr/bin/env bash
tezos_client_version="2020-03-11-ledger-patched"
config_template_link="https://raw.githubusercontent.com/StakerDAO/staker-dao/d9b84eecd013970c697392cae6bece750f22d117/scripts/config-template.yaml"

brew tap gpevnev/stakerdao
brew install gpevnev/stakerdao/tezos-client-patched

tezos_client="$(brew --cellar gpevnev/stakerdao/tezos-client-patched)/$tezos_client_version/bin/tezos-client"

brew install gpevnev/stakerdao/stkr-token-cli

config_dir="$HOME/.stkr-token-cli"
config_file="$config_dir/config.yaml"

mkdir -p "$config_dir"

rm "$config_file"

curl "$config_template_link" | sed "s%_TEZOS_CLIENT_PATH%$tezos_client%g" > $config_file

