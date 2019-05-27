let pkgs = import ./nix/nixpkgs;
    compiler = "ghc864";
    sourceRegex = [
      "^app.*$"
      "^data.*$"
      "^ormolu.cabal$"
      "^src.*$"
      "^tests.*$"
      "^.*\.md$"
    ];
    haskellPackages = pkgs.haskell.packages.${compiler}.override
      { overrides = (self: super:
          super //
          { "ormolu" = super.callCabal2nix "ormolu" (pkgs.lib.sourceByRegex ./. sourceRegex) {};
          });
      };
in if pkgs.lib.inNixShell
     then haskellPackages.shellFor
       { packages = (ps: [ ps.ormolu ]);
         buildInputs = [
           pkgs.cabal-install
         ];
       }
     else haskellPackages.ormolu
