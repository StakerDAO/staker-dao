# Staker DAO contract usage manual

This document covers management of contracts of
Staker DAO:

* Msig (Tezos)
* STKR (Tezos)
* BLEND (Ethereum)

All commands provided are specified for usage
on production. Note that config should specify
connection settings specific for mainnet.

## Tezos contracts

There are two contracts on Tezos blockchain:
Msig and STKR. Msig contract is being used as
a gateway for admin access to the STKR contract.
That is, Msig is an owner of STKR contract.

Internally, Msig holds public key hashes for
keys of Operations team.

Msig contract can't hold any funds (all transactions
should carry zero Tz coins).

STKR contract can be given funds with use of special
entrypoint `fund`.
Withdrawal of funds on STKR contract can be made
only by the Operations team using `withdraw`
entrypoint.

### Requirements

For interaction with contracts you are required to obtain:

* Latest build of `stkr-token-cli`
  * This tool can be built from master of STKR contract
    [git repository](http://todo-makelink.org) by following
    instructions from `README`
* Tezos client executable `tezos-client`
* Address of tezos full node

### CLI overview

STKR token CLI provides a number of commands.
Each command requires these arguments to be provided:

* Path to tezos client `--tzclient <path>`
  * Relative or absolute path to tezos client executable
* Tezos node address `-A <address>`
  * Network address of tezos node to connect to
* Tezos node port `-P <int>`
  * Port of tezos node

When describing usage of particular command we will
not mention these params, but only use `${env}`
placeholder. I.e.

```
env=" --tzclient <.. path to tezos client > -A <.. tezos node address ..> -P <.. tezos node port ..> "
```

In explanation below we will specify commands for
STKR token CLI and tezos client as if these executables
were installed in global path. Absolute or relative paths
can be used as well.

### Setup

This section describes how both Msig and STKR
contracts should be set up,
i.e. preconfigured and deployed.

#### Key generation

It should be decided, who are members of Operations
team. When deciding on number of Operation team
keys, it's highly not recommended to pick up even number.

There are some variations on how to allocate
Operations team keys, e.g.:

* Each Operations team member has a single key
* Three Operations team members hold a single key
  and one Operations team member (CEO) holds
  two keys.
  * This gives CEO an additional vote, so that
    for a decision execution he needs only one
    Operations team member to approve the decision.
* Two Operations team hold a single key each
  and CEO holds three keys.
  * This gives CEO ability to authorize any
    decision on his own.

If a scheme when some Operations team members hold
more than a single key, it's highly recommended to
keep each of these keys in separate keys. It's also
adviced that only one of these keys should be actively
used and other two to be put into cold storage
and accessed only in rare cases when situation
can not be resolved otherwise.

To generate a key, Operations team member should
use Tezos client:

```
./tezos-client gen keys my_stkr_ops_key --encrypted
```

Tezos client will ask for password to encrypt secret
key with. General advice for usage of secure passwords
applies here. It's highly not adviced to generate key
without `--encrypted` flag provided.

Label `my_stkr_ops_key` can be substituted with
any other string and serves as label for the key
in local key storage.

After key is generated, use the following command
to show corresponding public key hash (for to be
used in contract deployment later):

```
./tezos-client show address my_stkr_ops_key
```

This command will yield output like following, you
have to copy contents after `Hash: ` prefix,
`tz1YpsDuFe7CfuxYWHLNH3Y1aT9EY8xHi2pp` for the example:

```
Hash: tz1YpsDuFe7CfuxYWHLNH3Y1aT9EY8xHi2pp
Public Key: edpkvBMjZbecifs8JEAs8AkGDVJ6wp7xf5YwDqteJJ9PJWSd81wFEt
```

#### Council key generation

Council key generation is to be done using same
procedure as described above for Operations team
key generation.

#### Contract deployment

To deploy both contracts, use following command:

```
stkr-token-cli deploy ${env} --msigAlias msig --stkrAlias stkr --fromAlias payer --prod --start-year 2020 --team-pk-hash <..ops key hash 1..> --team-pk-hash <..ops key hash 2..> ..
```

Arguments `msig`, `stkr` are aliases under which
addresses of deployed contracts will be kept in tezos
client.
Alias `payer` is an alias for the key in tezos client
which holds Tz tokens to pay for transaction fees.

Specify as many `--team-pk-hash` arguments as there are
Operations team keys. For example for three keys full command
would look like following:

```
stkr-token-cli deploy ${env} --msigAlias msig --stkrAlias stkr --fromAlias payer --prod --start-year 2020 --team-pk-hash tz1LFLaayAWW6dYrzSX1SrnBzzMF9nZTF8QQ --team-pk-hash tz1c8JHqNnEg2WZ38GAxHjfttG6KqDX6wFTC --team-pk-hash tz1gePTw4hV33Xwu1sSpJmfpPVz9fej5hffx
```

Use following commands to print and copy addresses of `msig`
and `stkr` contracts:

```
> tezos-client ${env} show known contract stkr
KT1MLPFT5t59FJNMiVkftpPCNb35h45Jg2AH

> tezos-client ${env} show known contract msig
KT1P8Lb8MZA93Wd2nfJzZjRNM5LjB4q6KA2N
```

On a different laptop you will be able to execute following
commands to remember these addresses under same aliases:

```
./tezos-client ${env} remember contract stkr KT1MLPFT5t59FJNMiVkftpPCNb35h45Jg2AH
./tezos-client ${env} remember contract msig KT1P8Lb8MZA93Wd2nfJzZjRNM5LjB4q6KA2N
```

##### Testnet deployment

For deploying a contract in testnet, use following command.
It will generate a contract code that instead of 1-month epoch
will make each epoch consist of 4 fixed-duration stage.
Duration is specified in seconds.

In the command below you have to replace `${start}` with
timestamp in future. We recommend detting it 100s in future.
Current timestamp can be obtained on Linux/MacOS with `date +%s`.

```
stkr-token-cli deploy ${env} --msigAlias msig1 --stkrAlias stkr1 --fromAlias payer_tn --test --start ${start} --duration 600"
```

### Submit a proposal

As a first step, a proposal should be described as single YAML file.

Example of proposal file format is provided below:

```
description:
  My proposal (1)
newPolicy:
  urls:
    url1:
      - "be7663e0ef87d51ab149a60dfad4df5940d30395ba287d9907f8d66ce5061d96"
      - "https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf"
```

After that, each Operations team member should sign this proposal with his key.
Note, that signature procedure has to started from scratch if some other
administering operation occurred (and nonce of MSig contract changed).

To sign a submit new proposal operation, an Operations team member with
key alias `stkrOps1` should execute the following command:

```
stkr-token-cli new-proposal ${env} --stkrAlias stkr --msigAlias msig --msigKeyAlias stkrOps1 -p proposal.yaml --print-sigs
```

Which will emit output like:

```
edpkuC9nfKfZiojv2wGFm8Rnd8fVfCXHKeZeZsSTu7VhT2iM61CAjT:edsigtmiPTdbYfFHqADLCRanqY65SqUHYuGhZFb5Gr7iu9XQwGm79FtJnrdqkPQQpzGQa9VwjCYXmt2nytkrK8EULnFPMkRtBmm
```

This output should be collected from each Operations team
member and then the following command shall be executed from
a single laptop:

```
stkr-token-cli new-proposal ${env} --stkrAlias stkr --msigAlias msig -p proposal.yaml --fromAlias payer --msigSig <..sig1..> --msigSig <..sig2..>
```

Where instead of `<..sig1..>`, `<..sig2..>` outputs from Operations
team members should be substituted.

For example (for 2 keys of Operations team):

```
stkr-token-cli new-proposal ${env} --stkrAlias stkr --msigAlias msig -p proposal.yaml --fromAlias payer --msigSig edpkuC9nfKfZiojv2wGFm8Rnd8fVfCXHKeZeZsSTu7VhT2iM61CAjT:edsigtmiPTdbYfFHqADLCRanqY65SqUHYuGhZFb5Gr7iu9XQwGm79FtJnrdqkPQQpzGQa9VwjCYXmt2nytkrK8EULnFPMkRtBmm --msigSig edpkuv6YqCv8gukEpi7E6qCDNGLTBUJs4EbtnkZPtLAx59iU6cT3Mq:edsigu2LYjiunz6fvBbjV7omoKn9RA9KB4NSNJjU6ii1jkarWdiHaSuFsbgXAbzgzb6rAey5VRQp58knEvn4Sj5xJ3Hm94N4bDN
```

### Appoint a new council

As a first step, council members must generate secret keys and
share corresponding key hashes to Operations team.

Then each Operations team member should sign new council
appointement operation using the following command:

```
stkr-token-cli new-council ${env} --stkrAlias stkr --msigAlias msig --member <..council key hash 1..> --member <..council key hash 2..>  --msigKeyAlias stkrOps1 --print-sigs
```

For each council member, his key hash should be passed
to the command using `--member` option.

Output from each Operations team member should be collected
and then used in a command to execute new council command
on the network:

```
stkr-token-cli new-council ${env} --stkrAlias stkr --msigAlias msig --fromAlias payer --member <..council key hash 1..> --member <..council key hash 2..>  --msigSig <..sig1..> --msigSig <..sig2..> ..
```

Where instead of `<..sig1..>`, `<..sig2..>` outputs from Operations
team members should be substituted.


For example (for 2 keys of Operations team and 3 keys of Council):

```
stkr-token-cli new-council ${env} --stkrAlias stkr --msigAlias msig --fromAlias payer --member tz1LFLaayAWW6dYrzSX1SrnBzzMF9nZTF8QQ --member tz1c8JHqNnEg2WZ38GAxHjfttG6KqDX6wFTC --member tz1gePTw4hV33Xwu1sSpJmfpPVz9fej5hffx --msigSig edpkuC9nfKfZiojv2wGFm8Rnd8fVfCXHKeZeZsSTu7VhT2iM61CAjT:edsigtmiPTdbYfFHqADLCRanqY65SqUHYuGhZFb5Gr7iu9XQwGm79FtJnrdqkPQQpzGQa9VwjCYXmt2nytkrK8EULnFPMkRtBmm --msigSig edpkuv6YqCv8gukEpi7E6qCDNGLTBUJs4EbtnkZPtLAx59iU6cT3Mq:edsigu2LYjiunz6fvBbjV7omoKn9RA9KB4NSNJjU6ii1jkarWdiHaSuFsbgXAbzgzb6rAey5VRQp58knEvn4Sj5xJ3Hm94N4bDN
```

### Rotate Operations team keys

In order to change Operations team keys, a new set of key
hashes should be prepared and signed by majority of
the old Operations team keys.

Each Operations team member should sign new council
appointement operation using the following command:

```
stkr-token-cli rotate-msig-keys ${env} --msigAlias msig --team-pk-hash <..ops key hash 1..> --team-pk-hash <..ops key hash 2..> --msigKeyAlias stkrOps1 --print-sigs
```

Each key of new Operations team should be passed
to the command using `--tam-pk-hash` option.

Output from each Operations team member should be collected
and then used in a command to execute new council command
on the network:

```
stkr-token-cli rotate-msig-keys ${env} --msigAlias msig --fromAlias payer --team-pk-hash <..ops key hash 1..> --team-pk-hash <..ops key hash 2..> --msigSig <..sig1..> --msigSig <..sig2..> ..
```

Where instead of `<..sig1..>`, `<..sig2..>` outputs from Operations
team members should be substituted.


For example (for 2 keys of old Operations team and 3 keys of new Operations team):

```
stkr-token-cli rotate-msig-keys ${env} --msigAlias msig --fromAlias payer --team-pk-hash tz1LFLaayAWW6dYrzSX1SrnBzzMF9nZTF8QQ --team-pk-hash tz1c8JHqNnEg2WZ38GAxHjfttG6KqDX6wFTC --team-pk-hash tz1gePTw4hV33Xwu1sSpJmfpPVz9fej5hffx --msigSig edpkuC9nfKfZiojv2wGFm8Rnd8fVfCXHKeZeZsSTu7VhT2iM61CAjT:edsigtmiPTdbYfFHqADLCRanqY65SqUHYuGhZFb5Gr7iu9XQwGm79FtJnrdqkPQQpzGQa9VwjCYXmt2nytkrK8EULnFPMkRtBmm --msigSig edpkuv6YqCv8gukEpi7E6qCDNGLTBUJs4EbtnkZPtLAx59iU6cT3Mq:edsigu2LYjiunz6fvBbjV7omoKn9RA9KB4NSNJjU6ii1jkarWdiHaSuFsbgXAbzgzb6rAey5VRQp58knEvn4Sj5xJ3Hm94N4bDN
```

### Vote for a proposal

To vote for a proposal you need to know integer id of proposal (i.e. proposal sequential number) and an epoch in which this proposal is submitted.

To vote on behalf of a council member with key being kept encrypted
on a tezos client under alias `council1` for a proposal with sequential id
`7` submitted in epoch `3`, use following command:

(NB) Epoch numeration starts with `0` for January 2020.

```
stkr-token-cli vote ${env} --stkrAlias stkr --msigAlias msig --fromAlias payer -e 3 -p 7 --memberKeyAlias council1
```
