with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-unstable";
    rev = "7827d3f4497ed722fedca57fd4d5ca1a65c38256";
}) {};

let
  morleyRepo =
    builtins.fetchGit
    { url = https://gitlab.com/morley-framework/morley.git;
      ref = "master";
      rev = "3a545a894499e5457f814c5637e504c93422f548";
    };
  morleyPkgs = import "${morleyRepo}/pkgs.nix" {inherit haskell; inherit pkgs;};

  haskellPackages =
      with haskell.lib;
      haskell.packages.ghc865.override {
        overrides = self: super:
          { stkr-token = morleyPkgs.mkFromHpack super "stkr-token" ./. "stkr-token";
          } // (morleyPkgs.overrides self super);
      };

  drv = haskellPackages.stkr-token;

  hsTools = with haskellPackages; [
    cabal-install hpack
    hlint hdevtools
    morley
  ];

  dev = drv.env.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
               ++ hsTools;
  });
in
  if pkgs.lib.inNixShell then dev else drv
