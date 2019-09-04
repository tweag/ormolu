{ pkgs ? (import ./nix/nixpkgs) }:

let
  ormoluCompiler = "ghc865";
  source = pkgs.lib.sourceByRegex ./.[
    "^.*\.md$"
    "^app.*$"
    "^data.*$"
    "^ormolu.cabal$"
    "^src.*$"
    "^tests.*$"
    ];
  haskellPackages = pkgs.haskell.packages.${ormoluCompiler}.override {
    overrides = ormoluOverlay;
  };
  ormoluOverlay = self: super: {
    "ormolu" = super.callCabal2nix "ormolu" source { };
  };
  ormolize = import ./nix/ormolize {
    inherit pkgs;
    inherit haskellPackages;
  };
  ormolizedPackages = doCheck:
    pkgs.lib.mapAttrs (_: v: ormolize { package = v; inherit doCheck; }) haskellPackages;
in {
  ormolu = haskellPackages.ormolu;
  ormoluShell = haskellPackages.shellFor {
    packages = ps: [ ps.ormolu ];
    buildInputs = [ haskellPackages.cabal-install haskellPackages.ghcid ];
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
      "cassava"
      "conduit"
      "cryptonite"
      "diagrams-core"
      "distributed-process"
      "esqueleto"
      "fay"
      "haxl"
      "hedgehog"
      "hlint"
      "megaparsec"
      "ormolu"
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
      # "hakyll"
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
