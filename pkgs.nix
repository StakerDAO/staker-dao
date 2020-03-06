{ sources ? import ./nix/sources.nix
, haskell, pkgs }:

with pkgs;

let
  morleyRepo = sources.morley;
  morleyPkgs = import "${morleyRepo}/pkgs.nix" {inherit haskell pkgs;};

  haskellPackages =
      with haskell.lib;
      haskell.packages.ghc865.override {
        overrides = self: super:
          { stkr-token = morleyPkgs.mkFromHpack super "stkr-token" ./. "stkr-token";
          } // (morleyPkgs.overrides self super);
      };
in haskellPackages
