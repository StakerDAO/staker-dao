{ sources ? import ./nix/sources.nix }:
with (import sources.nixpkgs) {};

let
  haskellPackages = import ./pkgs.nix { inherit haskell pkgs; };
  package = haskellPackages.stkr-token;

  devEnv = package.env.overrideAttrs (attr: {
    nativeBuildInputs = with haskellPackages;
      [
        cabal-install hpack
        hlint hdevtools
        ];
        buildInputs = [
          morley
      ] ++ attr.buildInputs;

    shellHook =
      ''
        set -eu

        pushd stkr-token
        hpack
        cabal configure --extra-lib-dirs ${libsodium}/lib
        popd

        set +eu
      '';
  });
in
  if pkgs.lib.inNixShell then devEnv else package
