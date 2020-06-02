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
* Tezos node secure mode `-S` flag

When describing usage of particular command we will
not mention these params, but only use `${env}`
placeholder. I.e.

```
env=" --tzclient <.. path to tezos client > -A <.. tezos node address ..> -P <.. tezos node port ..> -S"
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
* Three Operations team members hold a single key each
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
can not be resolved in a different way.

To generate the key, Operations team member should
use Tezos client:

```
./tezos-client gen keys my_stkr_ops_key --encrypted
```

Tezos client will ask for password to encrypt secret
key with. General advice for usage of secure passwords
applies here. It's highly not adviced to generate the
key with no `--encrypted` flag provided.

`my_stkr_ops_key` is an alias the user gives to the key,
the user uses this alias to refer to the key when
executing commands.

Aliases is a general convenience mechanizm, which tezos
client provides to manage various entities the
user must refer to when working with tezos: contracts,
keys etc. These entities are globally addressable by
their hashes, but hashes may be quite cumbersome to work with.

Generally, the user may give any such entity the name (alias),
and use this name everywhere tezos client needs the entity
itself. The user may ask tezos client to print the
hash the given alias refers to.

While hashes are globally unique, aliases are local to
the given user account and have no meaning outside of it.

E.g. the following command shows corresponding public
key hash (for to be used in contract deployment later):

```
./tezos-client show address my_stkr_ops_key
```

This command yields output like the following, you
have to copy contents after `Hash: ` prefix,
`tz1YpsDuFe7CfuxYWHLNH3Y1aT9EY8xHi2pp` for the example:

```
Hash: tz1YpsDuFe7CfuxYWHLNH3Y1aT9EY8xHi2pp
Public Key: edpkvBMjZbecifs8JEAs8AkGDVJ6wp7xf5YwDqteJJ9PJWSd81wFEt
```

#### Council key generation

Council key generation is to be done using the same
procedure as described above for Operations team
key generation.

#### Contract deployment

To deploy both contracts, use the following command:

```
stkr-token-cli deploy ${env} --msigAlias msig --stkrAlias stkr --fromAlias payer --prod --start-year 2020 --supply 1500000 --team-pk-hash <..ops key hash 1..> --team-pk-hash <..ops key hash 2..> ..
```

Arguments `msig`, `stkr` are aliases for addresses of
deployed contracts.

Alias `payer` is an alias for the key which holds
Tz tokens to pay for transaction fees.

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

Conversely, you may create contract aliases for given contract addresses using
`remember` command:
```
./tezos-client ${env} remember contract stkr KT1MLPFT5t59FJNMiVkftpPCNb35h45Jg2AH
./tezos-client ${env} remember contract msig KT1P8Lb8MZA93Wd2nfJzZjRNM5LjB4q6KA2N
```

##### Testnet deployment

For deploying a contract in testnet, use the following command
(the contract is set to have a 4 fixed-duration stage epoch,
instead of the default one 1-month stage epoch):

```
stkr-token-cli deploy ${env} --msigAlias msig1 --stkrAlias stkr1 --fromAlias payer_tn --test --start ${start} --duration 600"
```
In the command above you have to replace `${start}` with
timestamp in future. We recommend setting it 100s in future.
Current timestamp can be obtained on Linux/MacOS with `date +%s`.

Duration is specified in seconds.


## Multisig commands

All multisig commands shall be signed by each Operations team member.
In current implementation `stkr-token-cli` expects either key alias or
the key:signature pair shall be supplied for each Operations team member.

Since alias is a local-to-account entity, to run multisig command using
key aliases, an account with aliases for Operations team member keys
shall exist and password for each key shall be entered during invocatin.

This may be extremely inconvenient to do. Thus all multisig commands have
extra `--print-sigs` option, which allows the signer to generate his
own copy of key:signature pair using only his own key alias and
command-specific data, not even making any call to tezos
network, thus not requiring `--from` option (which is required otherwise).

### Submit a proposal

The proposal is a single YAML file.

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

Each Operations team member should sign this proposal with his key.
Signing of the proposal uses MSig contract nonce, and all signatures
shall reference the same nonce, thus if any other administering operation
occurred during the signing process, the process shall be
restarted from scratch.

To sign and submit new proposal operation, the Operations team member with
key alias `stkrOps1` should execute the following command:

```
stkr-token-cli new-proposal ${env} --stkrAlias stkr --msigAlias msig --msigKeyAlias stkrOps1 -p proposal.yaml --print-sigs
```

Which will emit output like:

```
edpkuC9nfKfZiojv2wGFm8Rnd8fVfCXHKeZeZsSTu7VhT2iM61CAjT:edsigtmiPTdbYfFHqADLCRanqY65SqUHYuGhZFb5Gr7iu9XQwGm79FtJnrdqkPQQpzGQa9VwjCYXmt2nytkrK8EULnFPMkRtBmm
```

This output should be collected from each Operations team
member and then the following command shall be executed:

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

### Transfer tokens

To transfer STKR tokens from `from` to `receiver` Each Operations team member
should sign transfer operation using the following command:

```
stkr-token-cli transfer ${env} --stkrAlias stkr --msigAlias msig --fromAlias from --receiverAlias receiver --value v --msigKeyAlias stkrOps1 --print-sigs
```

Output from each Operations team member should be collected
and then used in a command to execute transfer command
on the network (signatures shall be supplied just like in the example below):

```
stkr-token-cli transfer ${env} --stkrAlias stkr --msigAlias msig --fromAlias from --receiverAlias receiver --value v --msigSig <..sig1..> --msigSig <..sig2..> ..  <..payer or reservoir..>
```

where `<..payer or reservoir..>` stands for one of `--payerAlias payer`, `--payer <address>` or `--reservoir` and indicates an address
from which STKR token will be withdrawn to be transfered to `receiver`.

As usual, `from` stands for a Tezos account which is responsible for paying transaction fees.

### Freeze STKR contract

To freeze a STKR contract Each Operations team member
should sign transfer operation using the following command:

```
stkr-token-cli freeze ${env} --stkrAlias stkr --msigAlias msig --msigKeyAlias stkrOps1 --print-sigs
```

Output from each Operations team member should be collected
and then used in a command to freeze the STKR contract
on the network (signatures shall be supplied just like in the above example):

```
stkr-token-cli freeze ${env} --stkrAlias stkr --msigAlias msig --msigSig <..sig1..> --msigSig <..sig2..>
```
Here `stkr` is an alias of the contract to be frozen.

### Set STKR successor contract

To set the successor of frozen STKR contract you need first ensure
the successor contract exists in Tezos network.

Each Operations team member should sign transfer operation using
the following command:

```
stkr-token-cli set-successor ${env} --stkrAlias stkr --succAlias succ --msigAlias msig --msigKeyAlias stkrOps1 --print-sigs
```

Output from each Operations team member should be collected
and then used in a command to set the successor of frozen STKR contract
on the network (signatures shall be supplied just like in the above example):

```
stkr-token-cli set-successor ${env} --stkrAlias stkr --succAlias succ --msigAlias msig --msigSig <..sig1..> --msigSig <..sig2..>
```

`--succAlias succ` option identifies existing STKR contract, which becomes
the successor contract of the frozen `stkr` contract.

Note, that `set-successor` is applicable only in the simplest case of
migrating STKR contract without altering its public interface.
In more complex cases of contract upgrade, developers of update have
to provide specific command for their case.

### Withdraw funds from STKR contract

To withdraw funds from the STKR contract Each Operations team member should
sign withdraw operation using the following command:


```
stkr-token-cli withdraw ${env} --stkrAlias stkr --fromAlias from --receiverAlias receiver --amount a --msigAlias msig --msigKeyAlias stkrOps1 --print-sigs
```

Output from each Operations team member should be collected
and then used in a command to withdraw funds from the STKR contract
on the network (signatures shall be supplied just like in the above example):

```
stkr-token-cli withdraw ${env} --stkrAlias stkr --fromAlias from --receiverAlias receiver --amount a --msigAlias msig --msigSig <..sig1..> --msigSig <..sig2..>
```

`--fromAlias from` indicates the address funds are withdrawed from, `--receiverAlias receiver`
indicates the reciever of funds, `--amount a` indicates an amount of μꜩ (mutez) tokens withdrawed.


## Other commands

### Vote for a proposal

To vote for a proposal you need to know integer id of proposal (i.e. proposal sequential number) and an epoch in which this proposal is submitted.

To vote on behalf of a council member with key being kept encrypted
on a tezos client under alias `council1` for a proposal with sequential id
`7` submitted in epoch `3`, use following command:

(NB) Epoch numeration starts with `0` for January 2020.

```
stkr-token-cli vote ${env} --stkrAlias stkr --fromAlias payer -e 3 -p 7 --memberKeyAlias council1
```

For convenience we also support a scenario resembling a multisig one. I'ts possible first do:

```
stkr-token-cli vote ${env} --stkrAlias stkr --fromAlias payer -e 3 -p 7 --print-sigs
```

record the signature printed (`msig`), and then do:

```
stkr-token-cli vote ${env} --stkrAlias stkr --fromAlias payer -e 3 -p 7 --memberSig <..msig..>
```

Note, that, unlike in multisig case, the first call is not entirely local (it queries the
contract storage), thus we still shall supply `--fromAlias` option in this case.

### Fund the STKR contract

The following `fund` command allows any `payer` to transfer some Tz coins
to an STKR contract.

Payload is not to be specified for the current version of contract and
is left intact for future support of paychecks.

```
stkr-token-cli fund ${env} --stkrAlias stkr --fromAlias payer --amount <amount> [ --payload <.. hex ..> ]
```

`payload` stands for arbitrary base16-encoded text (empty by default).
`amount` specifies amount of μꜩ (mutez) tokens to transfer

### Get balance of a STKR token address

The command below prints balance of STKR token address.

```
stkr-token-cli get-balance ${env} --stkrAlias stkr <..addr or reservoir..>
```

where `<..addr or reservoir..>` stands for either `--addrAlias addr` or `--reservoir`

### Get total supply of STKR

```
stkr-token-cli total-supply ${env} --stkrAlias stkr
```
