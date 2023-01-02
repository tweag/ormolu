{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.follows = "haskellNix/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (inputs.haskellNix) config;
          overlays = [ inputs.haskellNix.overlay ];
        };
        inherit (pkgs) lib haskell-nix;
        inherit (haskell-nix) haskellLib;

        defaultGHCVersion = "ghc925";
        ghcVersions = [ "ghc902" defaultGHCVersion ];
        perGHC = lib.genAttrs ghcVersions (ghcVersion:
          let
            hsPkgs = pkgs.haskell-nix.cabalProject {
              src = ./.;
              compiler-nix-name = ghcVersion;
              modules = [{
                packages.ormolu.writeHieFiles = true;
                packages.ormolu.components.exes.ormolu.preBuild =
                  lib.mkIf (self ? rev) ''export ORMOLU_REV=${self.rev}'';
              }];
            };
            inherit (hsPkgs.ormolu.components.exes) ormolu;
            hackageTests = import ./expected-failures { inherit pkgs ormolu; };
            regionTests = import ./region-tests { inherit pkgs ormolu; };
            fixityTests = import ./fixity-tests { inherit pkgs ormolu; };
            packages = lib.recurseIntoAttrs ({
              inherit ormolu;
              ormoluTests = haskellLib.collectChecks' hsPkgs;
              dev = { inherit hsPkgs; };
            } // hackageTests // regionTests // fixityTests
            // lib.optionalAttrs (ghcVersion == defaultGHCVersion) {
              inherit (hsPkgs.extract-hackage-info.components.exes) extract-hackage-info;
              weeder = pkgs.runCommand
                "ormolu-weeder"
                {
                  buildInputs = [ (hsPkgs.tool "weeder" "2.4.0") ];
                } ''
                mkdir -p $out
                export XDG_CACHE_HOME=$TMPDIR/cache
                weeder --config ${./weeder.dhall} \
                  --hie-directory ${hsPkgs.ormolu.components.library.hie} \
                  --hie-directory ${hsPkgs.ormolu.components.exes.ormolu.hie} \
                  --hie-directory ${hsPkgs.ormolu.components.tests.tests.hie}
              '';
            });
          in
          packages // {
            ci = pkgs.linkFarmFromDrvs "ormolu-ci-${ghcVersion}"
              (lib.attrValues (flake-utils.lib.flattenTree packages));
          });
        defaultGHC = perGHC.${defaultGHCVersion};

        binaries =
          let
            hsPkgs = defaultGHC.dev.hsPkgs.appendModule {
              modules = [{
                dontStrip = false;
                dontPatchELF = false;
                enableDeadCodeElimination = true;
              }];
            };
            ormoluExe = hsPkgs: hsPkgs.hsPkgs.ormolu.components.exes.ormolu;
          in
          lib.recurseIntoAttrs {
            Linux = ormoluExe hsPkgs.projectCross.musl64;
            macOS = pkgs.runCommand "ormolu-macOS"
              {
                nativeBuildInputs = [ pkgs.macdylibbundler ];
              } ''
              mkdir -p $out/bin
              cp ${ormoluExe hsPkgs}/bin/ormolu $out/bin/ormolu
              chmod 755 $out/bin/ormolu
              dylibbundler -b \
                -x $out/bin/ormolu \
                -d $out/bin \
                -p '@executable_path'
            '';
            Windows = ormoluExe hsPkgs.projectCross.mingwW64;
          };

        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
          };
        };
      in
      {
        packages = flake-utils.lib.flattenTree {
          inherit binaries pre-commit-check;
          default = defaultGHC.ormolu;
        };
        apps = {
          default = flake-utils.lib.mkApp {
            drv = defaultGHC.ormolu;
            exePath = "/bin/ormolu";
          };
          extract-hackage-info = flake-utils.lib.mkApp {
            drv = defaultGHC.extract-hackage-info;
            exePath = "/bin/extract-hackage-info";
          };
          format = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "ormolu-format";
              text = builtins.readFile ./nix/format.sh;
              runtimeInputs = [
                (defaultGHC.dev.hsPkgs.tool "cabal" "latest")
                defaultGHC.ormolu
              ];
            };
          };
        };
        devShells = {
          default = defaultGHC.dev.hsPkgs.shellFor {
            tools = {
              cabal = "latest";
              haskell-language-server = "latest";
            };
            withHoogle = false;
            exactDeps = false;
            inherit (pre-commit-check) shellHook;
          };
        };
        legacyPackages = defaultGHC // perGHC;
      });
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
