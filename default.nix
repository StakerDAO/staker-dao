rec {
  sources = import ./nix/sources.nix;
  haskellNixArgs = import sources."haskell.nix";
  pkgs = import sources.nixpkgs haskellNixArgs;
  haskell-nix = pkgs.haskell-nix;

  # haskell.nix package set
  hs-pkgs = import ./stkr-token { inherit pkgs; };

  # stkr-token package
  stkr-token = hs-pkgs.stkr-token;

  all-components = with stkr-token.components;
    [ library ] ++ pkgs.lib.attrValues exes ++ pkgs.lib.attrValues tests;
  run-test = stkr-token.checks.stkr-token-test;
  stkr-token-cli = stkr-token.components.exes.stkr-token-cli;
  test-end2end = stkr-token.components.exes.stkr-token-test-end2end;

  # nix-shell environment with installed dependencies
  shell = hs-pkgs.shellFor {
    packages = ps: [ ps.stkr-token ];
    buildInputs = [ haskell-nix.cabal-install ];

    # shell environment does not include packages listed
    # in 'build-tools', so they have to be built locally by cabal
    # https://github.com/input-output-hk/haskell.nix/issues/231
    exactDeps = false;
  };
}
