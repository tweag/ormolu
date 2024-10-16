{
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      # prevent nix-direnv from fetching stackage
      inputs.stackage.url = "github:input-output-hk/empty-flake";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    weeder = { url = "github:ocharles/weeder"; flake = false; };

    # for Ormolu Live
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
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

        ghcVersions = [ "ghc9101" "ghc984" "ghc9121" ];
        defaultGHCVersion = builtins.head ghcVersions;
        perGHC = lib.genAttrs ghcVersions (ghcVersion:
          let
            hsPkgs = pkgs.haskell-nix.cabalProject {
              src = ./.;
              compiler-nix-name = ghcVersion;
              modules = [{
                packages.ormolu.writeHieFiles = true;
                packages.extract-hackage-info.writeHieFiles = true;
                packages.ormolu.components.exes.ormolu.preBuild =
                  lib.mkIf (self ? rev) ''export ORMOLU_REV=${self.rev}'';
              }];
            };
            inherit (hsPkgs.ormolu.components.exes) ormolu;
            hackageTests = import ./expected-failures { inherit pkgs ormolu; };
            regionTests = import ./region-tests { inherit pkgs ormolu; };
            fixityTests = import ./fixity-tests { inherit pkgs ormolu; };
            weeder = hsPkgs.tool "weeder" { src = inputs.weeder; };
            packages = lib.recurseIntoAttrs ({
              inherit ormolu;
              ormoluTests = haskellLib.collectChecks' hsPkgs;
              dev = { inherit hsPkgs; };
            } // hackageTests // regionTests // fixityTests
            // lib.optionalAttrs (ghcVersion == defaultGHCVersion) {
              inherit (hsPkgs.extract-hackage-info.components.exes) extract-hackage-info;
              weeder = pkgs.runCommand "ormolu-weeder" { buildInputs = [ weeder ]; } ''
                mkdir -p $out
                weeder --config ${./weeder.toml} \
                  --hie-directory ${hsPkgs.ormolu.components.library.hie} \
                  --hie-directory ${hsPkgs.ormolu.components.exes.ormolu.hie} \
                  --hie-directory ${hsPkgs.ormolu.components.tests.tests.hie} \
                  --hie-directory ${hsPkgs.extract-hackage-info.components.exes.extract-hackage-info.hie}
              '';
            });
          in
          packages // {
            ci = pkgs.linkFarm "ormolu-ci-${ghcVersion}"
              (flake-utils.lib.flattenTree packages);
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
            linuxWindows = {
              native = ormoluExe hsPkgs.projectCross.musl64;
              windows = ormoluExe hsPkgs.projectCross.mingwW64;
            };
            macOS.native = pkgs.runCommand "ormolu-macOS"
              {
                nativeBuildInputs = [
                  pkgs.macdylibbundler
                  pkgs.darwin.autoSignDarwinBinariesHook
                ];
              } ''
              mkdir -p $out/bin
              cp ${ormoluExe hsPkgs}/bin/ormolu $out/bin/ormolu
              chmod 755 $out/bin/ormolu
              dylibbundler -b --no-codesign \
                -x $out/bin/ormolu \
                -d $out/bin \
                -p '@executable_path'
              signDarwinBinariesInAllOutputs
            '';
          in
          lib.recurseIntoAttrs
            (lib.optionalAttrs (system == "x86_64-linux") linuxWindows
              // lib.optionalAttrs pkgs.hostPlatform.isDarwin macOS);

        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            cabal-gild.enable = true;
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
          };
        };

        ormoluLive = import ./ormolu-live { inherit system inputs; };
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
                defaultGHC.ormolu
              ];
            };
          };
        };
        devShells = {
          default = defaultGHC.dev.hsPkgs.shellFor {
            tools = {
              cabal = "latest";
              haskell-language-server = {
                src = inputs.haskellNix.inputs."hls-2.9";
                configureArgs = "--disable-benchmarks --disable-tests";
              };
            };
            nativeBuildInputs = pre-commit-check.enabledPackages;
            withHoogle = false;
            exactDeps = false;
            inherit (pre-commit-check) shellHook;
          };
          ormoluLive = ormoluLive.shell;
        };
        legacyPackages = defaultGHC // perGHC;
      });
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://tweag-ormolu.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "tweag-ormolu.cachix.org-1:3O4XG3o4AGquSwzzmhF6lov58PYG6j9zHcTDiROqkjM="
    ];
  };
}
