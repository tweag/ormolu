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
in {
  ormolu = haskellPackages.ormolu;
  ormoluShell = haskellPackages.shellFor {
    packages = ps: [ ps.ormolu ];
    buildInputs = [ haskellPackages.cabal-install haskellPackages.ghcid ];
  };
  inherit ormoluOverlay ormoluCompiler;
  hackage = pkgs.lib.mapAttrs ormolize haskellPackages;
}
