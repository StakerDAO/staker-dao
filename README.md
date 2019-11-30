<!--
   - SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: LicenseRef-ReplaceMe
   -->

# StakerDAO

StakerDAO: implementation of STKR and STKRC contracts.

## Quick-start instruction

Within this instruction Serokell's Tezos node at
`jupiter.serokell.io:8732` is used with tezos client.

1. Build and copy `tezos-client` into current directory:

```
cp <...>/bin/tezos-client .
```

2. Build `staker-dao` project:

```
make build
```

3. Go to [faucet](https://faucet.tzalpha.net/) and download
wallet file, save it to `./alpha-faucet-wallet.json`, import it into
tezos client and copy the output after `Hash:`:

```
./tezos-client -A jupiter.serokell.io -P 8732 activate account myaccount with ./alpha-faucet-wallet.json
./tezos-client -A jupiter.serokell.io -P 8732 show address myaccount | grep Hash:
```

Which will result in message like:

```
Warning:

                 This is NOT the Tezos Mainnet.

     The node you are connecting to claims to be running on the
               Tezos Alphanet DEVELOPMENT NETWORK.
          Do NOT use your fundraiser keys on this network.
          Alphanet is a testing network, with free tokens.

Hash: tz1PPPYChg5xXHpGzygnNkmzPd1hyVRMxvJf
```

Alternatively you can take one of keys mentioned on
[Notion page](https://www.notion.so/serokell/Meta-Tezos-95973ecb1c3449e6859a3720af81c6fb).

4. Originate contract:

```
 ./result/bin/stkr-token-exe deploy --tzclient ../tezos-client -A jupiter.serokell.io -P 8732 --contractName mycontract --from tz1PPPYChg5xXHpGzygnNkmzPd1hyVRMxvJf
```

You will see output like:

```
Contract addr: KT1SitwnwEDAQRbznuo4xJaKtQbfJ3XQTPbB
```

You can find deployed contract on [block explorer](https://babylonnet.tezos.id).

## Build Instructions [↑](#-StakerDAO)

Run `make build` to build everything.

## Issue Tracker [↑](#-StakerDAO)

We use [YouTrack](https://issues.serokell.io/issues/SDAO) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors [↑](#-StakerDAO)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
