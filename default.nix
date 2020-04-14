{ pkgs ? (import ./nix/nixpkgs)
, ormoluCompiler ? "ghc883" # the shell doesn't work with ghc8101 yet
}:

let
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
    "ormolu" = super.callCabal2nixWithOptions "ormolu" source "-fdev" { };
    "ghc-lib-parser" = pkgs.haskell.lib.dontHaddock
      (super.callHackageDirect {
        pkg = "ghc-lib-parser";
        ver = "8.10.1.20200412";
        sha256 = "sha256-EjMzp8xRT3xVFKDI1fAfopkylGB0hv35Av2+uZeETRU=";
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
  ormoluShell =
    if ormoluCompiler == "ghc8101"
      # HACK The shell doesn't compile with GHC 8.10.1
      then haskellPackages.shellFor {
        packages = ps: [];
        buildInputs = [];
      }
      else haskellPackages.shellFor {
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

      # Uses CPP to concatenate strings in a deprecation annotation

      # "esqueleto"

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
