{ pkgs }:

with rec {
  # clean gitignored files
  projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "staker-dao";
    src = ../.;
  };

  # filter for files needed to run hpack
  filesForHpack = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    name = "files-for-hpack";
    src = projectSrc;
    filter = path: type:
      let
        relPath = pkgs.lib.removePrefix (toString projectSrc.origSrc + "/") (toString path);
        regexes = [ "hpack" "hpack/.*" "stkr-token" "stkr-token/.*" ];
      in pkgs.lib.any (re: builtins.match re relPath != null) regexes;
  };

  # haskell.nix does not support package.yaml files referencing
  # parent directectories, so we generate the .cabal file ourselves
  # https://github.com/input-output-hk/haskell.nix/issues/485
  cabalFile = pkgs.runCommand "stkr-token.cabal" {} ''
    ${pkgs.haskellPackages.hpack}/bin/hpack ${filesForHpack}/stkr-token - > $out
  '';

  # package src with .cabal file added
  srcWithCabalFile = pkgs.runCommand "src-with-cabal" {} ''
    cp -r --no-preserve=mode ${projectSrc}/stkr-token $out
    cp ${cabalFile} $out/stkr-token.cabal
  '';

  hsPkgs = pkgs.haskell-nix.stackProject {
    src = srcWithCabalFile;

    ignorePackageYaml = true;

    modules = [{
      packages.stkr-token.package.ghcOptions = "-Werror -O0";

      # don't haddock dependencies
      doHaddock = false;
      packages.stkr-token.doHaddock = true;
    }];
  };
};

hsPkgs
