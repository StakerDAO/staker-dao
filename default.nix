with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-unstable";
    rev = "7827d3f4497ed722fedca57fd4d5ca1a65c38256";
}) {};

let
  haskellPackages = import ./pkgs.nix {inherit haskell; inherit pkgs;};
  drv = haskellPackages.stkr-token;

  hsTools = with haskellPackages; [
    cabal-install hpack
    hlint hdevtools
    morley
  ];

  dev = drv.env.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
               ++ hsTools;
    shellHook =
      ''
        cd stkr-token && hpack && cabal configure --extra-lib-dirs ${libsodium}/lib
      '';
  });
in
  if pkgs.lib.inNixShell then dev else drv
