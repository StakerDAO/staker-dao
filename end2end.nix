{ sources ? import ./nix/sources.nix
, useTezosDerivation ? true
, tezosClientPath ? "tezos-client" }:

with (import sources.nixpkgs) {};

let
  tezosPackaging = (import sources.tezos-packaging) {};

  stkr-token-pkgs = import ./pkgs.nix {inherit haskell pkgs;};
  drv = stkr-token-pkgs.stkr-token;

  tezosClientStatic = tezosPackaging.tezos-client-static;

  TEZOS_CLIENT =
    if useTezosDerivation
    then "${tezosClientStatic}/bin/tezos-client"
    else tezosClientPath;


  testScript = writeShellScript "stkr-token-test-end2end-script" ''
    export TEZOS_CLIENT=${TEZOS_CLIENT}
    ${drv}/bin/stkr-token-test-end2end \
      ${./stkr-token/tezos-nodes/serokell.yaml} \
      ${./stkr-token/test-accounts/faucet.yaml} \
      300
  '';

in testScript
