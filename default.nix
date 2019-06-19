let
  pkgs = import ./nix/nixpkgs;
  gitignoreSource = import ./nix/gitignore { inherit (pkgs) lib; };
  compiler = "ghc864";
  source = gitignoreSource ./.;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = (self: super:
    super // {
      "ormolu" = super.callCabal2nix "ormolu" source { };
    });
  };
in {
  ormolu = haskellPackages.ormolu;
  ormolu-shell = haskellPackages.shellFor {
    packages = ps: [ ps.ormolu ];
    buildInputs = [ pkgs.cabal-install ];
  };
}

