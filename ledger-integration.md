# Using Ledger to sign council votes and multisig packages

This is instruction on how to setup Ledger (in my case Ledger Nano S) with tezos account secret key used as signer key.

## 1. Basic Ledger setup

Setup your PIN and recovery phrases following instructions on [offical site](https://support.ledger.com/hc/en-us/articles/360000613793-Set-up-as-new-device).

## 2. Tezos app on Ledger Live

1. Install Ledger Live desktop application from [offical site](https://www.ledger.com/ledger-live/download/)

2. Open Ledger Live and perform initial setup following instructions in app

3. Enter _Manager_ tab and search for _Tezos Wallet_ application in _App Catalog_

4. Install _Tezos Wallet_ app

5. After app is installed Ledger Live will suggest you to add new account

6. Follow instructions on Ledger Live and add at least one account with specified name

## 3. Import your wallet from app on ledger to `tezos-client`

1. Open Terminal with `tezos-client` on `PATH`.

2. Execute `tezos-client list connected ledgers`. This will produce you output similar to this:
    ```
    tezos-client import secret key ledger_user "ledger://major-squirrel-thick-hedgehog/bip25519"j## Ledger `major-squirrel-thick-hedgehog`
    Found a Tezos Wallet 2.1.0 (git-description: "091e74e9") application running
    on Ledger Nano S at
    [IOService:/AppleACPIPlatformExpert/PCI0@0/AppleACPIPCI/XHC1@14/XHC1@14000000/HS03@14300000/Nano
    S@14300000/Nano S@0/IOUSBHostHIDDevice@14300000,0].

    To use keys at BIP32 path m/44'/1729'/0'/0' (default Tezos key path), use one
    of:

    tezos-client import secret key ledger_username "ledger://major-squirrel-thick-hedgehog/bip25519/0h/0h"
    tezos-client import secret key ledger_username "ledger://major-squirrel-thick-hedgehog/ed25519/0h/0h"
    tezos-client import secret key ledger_username "ledger://major-squirrel-thick-hedgehog/secp256k1/0h/0h"
    tezos-client import secret key ledger_username "ledger://major-squirrel-thick-hedgehog/P-256/0h/0h"
    ```

3. Import one of keys from ledger using commands listed on previous command output.

   For example: `tezos-client import secret key ledger_user "ledger://major-squirrel-thick-hedgehog/bip25519"`

    Where `ledger_user` is an alias for this key stored on ledger and can be used as regular alias for `tezos-client` and thus  `stkr-token-cli`

## 4. Obtain your wallet address using `tezos-client`
In Terminal run command `tezos-client show address ledger_user` which would produce output in format:
```
Hash: tz1SuEv4GkjNHFHSg1jjRB2g4s56GFo1JK26
Public Key: edpkvQCE88Y8vWT9qJXHigsyKve4P4kjfDtBUGfkZnLJyCpzbgJVEo
```

Where hash of your public key or `tz1SuEv4GkjNHFHSg1jjRB2g4s56GFo1JK26` is your wallet address from Ledger.