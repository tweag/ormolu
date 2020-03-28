{ pkgs ? (import ./nix/nixpkgs) }:

let
  ormoluCompiler = "ghc865";
  source = pkgs.lib.sourceByRegex ./. [
    "^.*\.md$"
    "^app.*$"
    "^data.*$"
    "^ormolu\.cabal$"
    "^src.*$"
    "^tests.*$"
    ];
  haskellPackages = pkgs.haskell.packages.${ormoluCompiler}.override {
    overrides = ormoluOverlay;
  };
  ormoluOverlay = self: super: {
    "ormolu" = pkgs.haskell.lib.enableCabalFlag
      (super.callCabal2nix "ormolu" source { }) "dev";
    # Nixpkgs provides ghc-lib-parser-8.8.0.20190424, but we want
    # ghc-lib-parser-8.10.1. We disable Haddock generation because it's way
    # too slow.
    "ghc-lib-parser" = pkgs.haskell.lib.dontHaddock
      (super.callHackageDirect {
        pkg = "ghc-lib-parser";
        ver = "8.10.1.20200324";
        sha256 = "0f2c68fzdj2lw6da2zpx7a0cx631im3kylwd85dzx1npsm1vzlbg";
      } {});
  };
  ormolize = import ./nix/ormolize {
    inherit pkgs;
    inherit haskellPackages;
  };
  # NOTE We have to exclude some directories for some packages because
  # Ormolu needs files to be parsable by Haddock which is not always the
  # case. For example some tests and examples do not parse.
  excludedHackageDirs = {
    "aws" = ["Examples"];
    "distributed-process" = ["benchmarks"];
    "esqueleto" = ["test"];
    "fay" = ["examples"];
    "postgrest" = ["test"];
  };
  ormolizedPackages = doCheck:
    pkgs.lib.mapAttrs (name: p: ormolize {
      package = p;
      inherit doCheck;
      excludedDirs =
        if builtins.hasAttr name excludedHackageDirs
          then excludedHackageDirs.${name}
          else [];
    }) haskellPackages;
in {
  ormolu = haskellPackages.ormolu;
  ormoluShell = haskellPackages.shellFor {
    packages = ps: [
      ps.ormolu
    ];
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  };
  withOrmolu = haskellPackages.shellFor {
    packages = ps: [];
    buildInputs = [
      haskellPackages.ormolu
    ];
  };
  inherit ormoluOverlay ormoluCompiler;
  hackage = ormolizedPackages false;
  hackageTests = with pkgs.lib; pkgs.recurseIntoAttrs (
    let ps = [
      "QuickCheck"
      "ShellCheck"
      "aeson"
      "attoparsec"
      "aws"
      "capability"
      "cassava"
      "conduit"
      "cryptonite"
      "diagrams-core"
      "distributed-process"
      "esqueleto"
      "fay"
      "hakyll"
      "haxl"
      "hedgehog"
      "hlint"
      "megaparsec"
      "ormolu"
      "optics"
      "postgrest"
      "servant"
      "servant-server"
      "tensorflow"
      "text_1_2_4_0"
      "tls"
      "yesod-core"

      # Comment idempotence issue

      # "Agda"
      # "brick"
      # "hledger"
      # "http-client"
      # "idris"
      # "intero"
      # "leksah"
      # "pandoc"
      # "pipes"
      # "stack"

      # Missing language extension

      # "lens" #fixed in master
      # "purescript"

    ];
    in listToAttrs (map (p: nameValuePair p (ormolizedPackages true).${p}) ps)
  );
}
