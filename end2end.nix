{ sources ? import ./nix/sources.nix
, useTezosDerivation ? true
, tezosClientPath ? "tezos-client" }:

with (import sources.nixpkgs) {};

let
  tezosPackaging = (import sources.tezos-packaging) { pkgs = (import sources.nixpkgs) {}; };

  drv = (import ./default.nix).test-end2end;

  tezosClientStatic = tezosPackaging.binaries-drv;

  TEZOS_CLIENT =
    if useTezosDerivation
    then "${tezosClientStatic}/bin/tezos-client"
    else tezosClientPath;

  testConfig = writeTextFile {
    name = "test-config.yaml";
    text = ''
      envTezosClientCmd: "${TEZOS_CLIENT}"
      envNodeAddr:
        naHost: "carthage.testnet.tezos.serokell.team"
        naPort: 8732
    '';
  };


  testScript = writeShellScript "stkr-token-test-end2end-script" ''
    ${drv}/bin/stkr-token-test-end2end \
      ${testConfig} \
      ${./stkr-token/test-accounts/faucet.yaml} \
      300
  '';

in testScript
