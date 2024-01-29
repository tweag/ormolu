{
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      # prevent nix-direnv from fetching stackage
      inputs.stackage.url = "github:input-output-hk/empty-flake";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # for Ormolu Live
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
    npmlock2nix = { url = "github:nix-community/npmlock2nix"; flake = false; };
    ps-tools = {
      follows = "purs-nix/ps-tools";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    purs-nix = {
      url = "github:purs-nix/purs-nix/ps-0.15";
      inputs.nixpkgs.follows = "nixpkgs";
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

        ghcVersions = [ "ghc963" "ghc947" "ghc981" ];
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
            weeder = hsPkgs.tool "weeder" {
              version = "2.6.0";
              modules = [{ reinstallableLibGhc = false; }];
            };
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
              Linux = ormoluExe hsPkgs.projectCross.musl64;
              Windows = ormoluExe hsPkgs.projectCross.mingwW64;
            };
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
          in
          lib.recurseIntoAttrs
            (lib.optionalAttrs (system == "x86_64-linux") linuxWindows
              // lib.optionalAttrs (system == "x86_64-darwin") { inherit macOS; });

        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            purs-tidy.enable = true;
          };
          tools = { inherit (ormoluLive) purs-tidy; };
        };

        ormoluLive = import ./ormolu-live {
          inherit pkgs inputs defaultGHC;
        };
      in
      {
        packages = flake-utils.lib.flattenTree {
          inherit binaries pre-commit-check;
          default = defaultGHC.ormolu;
          ormoluLive = ormoluLive.package;
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
              haskell-language-server = {
                src = inputs.haskellNix.inputs."hls-2.4";
                configureArgs = "--disable-benchmarks --disable-tests";
              };
            };
            withHoogle = false;
            exactDeps = false;
            inherit (pre-commit-check) shellHook;
          };
          ormoluLive = ormoluLive.shell;
          ghcWasm = ormoluLive.ghcWasmShell;
        };
        legacyPackages = defaultGHC // perGHC;
      });
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://tweag-ormolu.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "tweag-ormolu.cachix.org-1:3O4XG3o4AGquSwzzmhF6lov58PYG6j9zHcTDiROqkjM="
    ];
  };
}
