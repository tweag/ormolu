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

    # for Ormolu Live
    ghc-wasm-meta.url = "github:haskell-wasm/ghc-wasm-meta/4e1f900e9933966634bc2e29dbeb81d09ce36727";
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

        ghcVersions = [ "ghc9103" "ghc9124" "ghc9141" ];
        defaultGHCVersion = builtins.head ghcVersions;

        cleanSrc = haskellLib.cleanSourceWith {
          name = "ormolu-src";
          src = ./.;
          filter = path: _type:
            let relPath = lib.removePrefix (toString ./. + "/") (toString path); in
            lib.any (prefix: relPath == prefix || lib.hasPrefix "${prefix}/" relPath) [
              "src"
              "app"
              "tests"
              "data"
              "extract-hackage-info"
            ]
            || lib.elem relPath [
              "cabal.project"
              "ormolu.cabal"
            ];
        };
        docStubs = [
          "LICENSE.md"
          "CHANGELOG.md"
          "CONTRIBUTING.md"
          "DESIGN.md"
          "README.md"
        ];
        src = pkgs.runCommand "ormolu-src-with-doc-stubs" { } ''
          cp -r ${cleanSrc} $out
          chmod -R u+w $out
          ${lib.concatMapStringsSep "\n" (f: "touch $out/${f}") docStubs}
        '';

        perGHC = lib.genAttrs ghcVersions (ghcVersion:
          let
            hsPkgs = pkgs.haskell-nix.cabalProject {
              inherit src;
              compiler-nix-name = ghcVersion;
              modules = [{
                packages.ormolu.writeHieFiles = true;
                packages.extract-hackage-info.writeHieFiles = true;
              }];
            };
            inherit (hsPkgs.ormolu.components.exes) ormolu;
            cpphs = hsPkgs.tool "cpphs" "1.20.10";
            hackageTests = import ./expected-failures { inherit pkgs ormolu cpphs; };
            regionTests = import ./region-tests { inherit pkgs ormolu; };
            fixityTests = import ./fixity-tests { inherit pkgs ormolu; };
            weeder = hsPkgs.tool "weeder" "2.10.0";
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
                packages.ormolu.components.exes.ormolu.preBuild =
                  lib.mkIf (self ? rev) ''export ORMOLU_REV=${self.rev}'';
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
              // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin macOS);

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
