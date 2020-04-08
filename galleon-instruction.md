# Instruction on STKR contract token functionality for Galleon team

This instruction provides a quick introduction on how to transfer STKR tokens on Tezos testnet.
It is more like a wrapper around [MANUAL](https://github.com/StakerDAO/staker-dao/blob/master/MANUAL.md) for Galleon team.

## 1. Generate team keys for signing transfers

Using `tezos-client` and following instructions in _Key generation_ [section in MANUAL](https://github.com/StakerDAO/staker-dao/blob/master/MANUAL.md) you need to geneate at least 3 PK/SK pairs using `tezos-client gen keys`.

For example lets generate keys with names `galleon_team_{1, 2, 3}`.
```bash
tezos-client gen keys galleon_team_1
tezos-client gen keys galleon_team_2
tezos-client gen keys galleon_team_3
```
## 2. Deploy Msig and STKR contracts using `stkr-token-cli`

This section is already covered pretty well in [MANUAL](https://github.com/StakerDAO/staker-dao/blob/master/MANUAL.md#contract-deployment), so I will just explain it for our example.

Before deploying we need to know pulic key hashes for `galleon_team_{1, 2, 3}`.
For this we would need to run `tezos-client show address`.
In my case for example `tezos-client show address galleon_team_1` would produce:
```
Hash: tz1Rsrmcinoo5zDiGxbXAE9wPLSy57CsQYnT
Public Key: edpkv8WsAeoZtVuRQ9WQEnGKeXmxFPKdhf68m5m7VydGVZYqeCzxNj
```
where `tz1Rsrmcinoo5zDiGxbXAE9wPLSy57CsQYnT` is hash of public key, which we would need on the next step.

Now lets use these public key hashes as our `--team-pk-hash`'es during deployment.

```
stkr-token-cli 'deploy' -c ~/.stkr-token-cli/config.yaml --msigAlias galleon_msig \
--stkrAlias galleon_stkr --fromAlias faucet \
--team-pk-hash tz1TrKSqdoaxLoRgNVDc6wqzreoV6bnDsfRw \
--team-pk-hash tz1d5jCBQdY1FmAbPX8qddD6WMkvEuJ8888y \
--team-pk-hash tz1Rsrmcinoo5zDiGxbXAE9wPLSy57CsQYnT \
--prod --start-year 2020 --supply 10000000
```
In my example this command produced folowing output:
```
Deploy result: {token: KT1N3YaxhH3JGr3u9Q7ULd6MnMxYo24jKKDF, multisig: KT1BRfaWPg4PyVJgi7yvf2MUkE9KgY2DmkdU}
```

## 3. Obtaining deposited amount on specific address

By default after deployment all supply for tokens are hold at _reservoir accout_ which is
just a hardcoded account with key hash equals `tz1WAVpSaCFtLQKSJkrdVApCQC1TNK8iNxq9`.
Let's find out it's balance using `tezos-client`.

### Looking at contract storage

Firstly we need to obtain STKR contract storage:
`tezos-client get contract storage for galleon_stkr`
Output:
```
Pair (Pair (Pair "KT1BRfaWPg4PyVJgi7yvf2MUkE9KgY2DmkdU" {}) (Pair {} (Pair {} {})))
     (Pair (Pair 0 10000000) (Pair 959 (Pair False None)))
```

The deposit values themselves are stored in a `big map` and thus not shown at storage, but can be obtained using _big map id_, which is equal to `959` in this example.

### Retrieving values from `big map`

In order to retrieve key from `big map` `tezos-client get element <key> of big map <id>` is used, where `<id>` is our _big map id_ from previous step and `<key>` is hashed literal of needed address which can be obtained with command `tezos-cleint hash data '"<account>"' of type address`, where `<account>` is ledger account, which we are trying to access.

For example for _reservoir address_ the command would be following:
`tezos-client hash data '"tz1WAVpSaCFtLQKSJkrdVApCQC1TNK8iNxq9"' of type address`
Producing:
```
Raw packed data: 0x050a0000001600007374616b65722d64616f2f7265736572766f6972
Script-expression-ID-Hash: exprucaLf6G5Robew77nwXRNR7gAbUJ3da2yAwqJdhyZCXZdEkLz8t
Raw Script-expression-ID-Hash: 0x90a1e4c60399662d6c3984dfa3bad6707dd8239f92a4c5ee07aedcd546531cbd
Ledger Blake2b hash: AjatWN7ZXjNyUSZTStxkpXu9YSVEdCEZMW3HwiCophtx
Raw Sha256 hash: 0x621eb15738275ed40492a81e281c8a848d1634ad4710bd02073f4d763687cd3e
Raw Sha512 hash: 0x32fd266366c12d66551daa14473fbfdec0e7453341db81ac61f2fd95f35ae5fae9ce0d84a81aaeeb282a6c81821a0f45438b52529efea518ab438fec350ee100
Gas remaining: 1039901 units remaining
```

Then using produced `Script-expression-ID-Hash` we can finaly retrive value from `big map`:
`tezos-client get element "exprucaLf6G5Robew77nwXRNR7gAbUJ3da2yAwqJdhyZCXZdEkLz8t" of big map 959`
Output: `10000000`
