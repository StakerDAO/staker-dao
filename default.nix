{sources ? import ./nix/sources.nix }:

with (import sources.nixpkgs) {};

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
