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
  expectedFailures = [
    "Agda"
    "aws"
    "brick"
    "distributed-process"
    "esqueleto"
    "fay"
    "idris"
    "intero"
    "leksah"
    "lens"
    "pandoc"
    "pipes"
    "purescript"
    "stack"
  ];
  ormolizedPackages = doCheck:
    pkgs.lib.mapAttrs (name: p: ormolize {
      package = p;
      inherit doCheck;
      expectedFailures =
        if pkgs.lib.lists.any (x: x == name) expectedFailures
          then ./expected-failures + "/${name}.txt"
          else null;
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
      "Agda"
      "QuickCheck"
      "ShellCheck"
      "aeson"
      "attoparsec"
      "aws"
      "brick"
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
      "hledger"
      "hlint"
      "http-client"
      "idris"
      "intero"
      "leksah"
      "lens"
      "megaparsec"
      "optics"
      "ormolu"
      "pandoc"
      "pipes"
      "postgrest"
      "purescript"
      "servant"
      "servant-server"
      "stack"
      "tensorflow"
      "text_1_2_4_0"
      "tls"
      "yesod-core"
    ];
    in listToAttrs (map (p: nameValuePair p (ormolizedPackages true).${p}) ps)
  );
}
