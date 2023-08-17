{ pkgs, ormolu }:

let
  inherit (pkgs) lib;
  expectedFailures = [
    "Agda"
    "brittany"
    "esqueleto"
    "hlint"
    "leksah"
    "lens"
    "pandoc"
    "pipes"
    "postgrest"
    "purescript"
  ];
  ormolizedPackages =
    let
      ormolize = import ../nix/ormolize { inherit pkgs ormolu; };
      ormolizeOverlay = _self: _super: { };
      ormolizablePackages = pkgs.haskellPackages.override {
        overrides = ormolizeOverlay;
      };
    in
    doCheck: lib.mapAttrs
      (name: p: ormolize {
        package = p;
        inherit doCheck;
        expectedFailures =
          if lib.lists.any (x: x == name) expectedFailures
          then ./. + "/${name}.txt"
          else null;
      })
      ormolizablePackages;
in
{
  hackage = ormolizedPackages false;
  hackageTests =
    let
      ps = [
        "Agda"
        "QuickCheck"
        "ShellCheck"
        "aeson"
        "attoparsec"
        "aws"
        "brick"
        "brittany"
        "capability"
        "cassava"
        "conduit"
        "cryptonite"
        "diagrams-core"
        "distributed-process"
        "esqueleto"
        "fay"
        "graphql-engine"
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
        "pandoc"
        "parsec3"
        "pipes"
        "postgrest"
        "purescript"
        "raaz"
        "servant"
        "servant-server"
        "stack"
        "tensorflow"
        "text_2_0_2"
        "tls"
        "unpacked-containers"
        "yesod-core"
      ];
    in
    pkgs.recurseIntoAttrs (lib.genAttrs ps (p: (ormolizedPackages true).${p}));
}
