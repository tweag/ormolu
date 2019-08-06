let
  pkgs = import ./nix/nixpkgs;
  compiler = "ghc864";
  source = pkgs.lib.sourceByRegex ./.[
    "^.*\.md$"
    "^app.*$"
    "^data.*$"
    "^ormolu.cabal$"
    "^src.*$"
    "^tests.*$"
    ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = (self: super:
    super // {
      "ormolu" = super.callCabal2nix "ormolu" source { };
    });
  };
  ormolize = import ./nix/ormolize {
    inherit pkgs;
    inherit haskellPackages;
  };
in {
  ormolu = haskellPackages.ormolu;
  ormolu-shell = haskellPackages.shellFor {
    packages = ps: [ ps.ormolu ];
    buildInputs = [ haskellPackages.cabal-install haskellPackages.ghcid ];
  };
  hackage = pkgs.lib.mapAttrs ormolize haskellPackages;
}
