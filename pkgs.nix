{ haskell, pkgs }:

with pkgs;

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
in haskellPackages
