with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-unstable";
    rev = "7827d3f4497ed722fedca57fd4d5ca1a65c38256";
}) {};

{ useTezosDerivation ? true, tezosClientPath ? "tezos-client" }:

let
  tezosPackaging = import (fetchFromGitHub {
    owner = "serokell";
    repo = "tezos-packaging";
    rev = "7a50817b9e98845f726be842b260396c79f33eda";
    sha256 = "1s9pd7mfr5b55jnlzrkjw1kn76n52rydslmbkzg3h8p8js4nnadj";
  });

  stkr-token-pkgs = import ./pkgs.nix {inherit haskell; inherit pkgs;};
  drv = stkr-token-pkgs.stkr-token;

  tezosClientStatic = (tezosPackaging {}).tezos-client-static;

  TEZOS_CLIENT =
    if useTezosDerivation
    then "${tezosClientStatic}/bin/tezos-client"
    else tezosClientPath;


  testScript = writeScript "stkr-token-test-end2end-script" ''
    export TEZOS_CLIENT=${TEZOS_CLIENT}
    ${drv}/bin/stkr-token-test-end2end \
      ${./stkr-token/tezos-nodes/cryptium-labs.yaml} \
      ${./stkr-token/test-accounts/faucet.yaml} \
      300
  '';

in testScript
