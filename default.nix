{ ormoluCompiler ? "ghc8107",
  ormoluLiveLink ? true
}:

let
  pkgs = import ./nix/pkgs.nix;
  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "ormolu";
      src = ./.;
      keepGitDir = true;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = ormoluCompiler;
    modules =
      let
        gitTH = name: baseDir: { pkgs, lib, ... }: {
          packages."${name}".components.exes."${name}" = {
            build-tools =
              lib.mkForce [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];
            extraSrcFiles = [ "${baseDir}.git/**/*" ];
          };
        };
      in [
        ({ lib, ... }: {
          config = {
            dontStrip = false;
            dontPatchELF = false;
            enableDeadCodeElimination = true;
            packages.ormolu.writeHieFiles = true;
          };
          # Make Cabal reinstallable
          options.nonReinstallablePkgs =
            # See https://github.com/input-output-hk/haskell.nix/issues/1177
            let adapt = ps: if lib.hasPrefix "ghc9" ormoluCompiler
                            then ps ++ [ "exceptions" "stm" ] else ps;
            in lib.mkOption { apply = ps: adapt (lib.remove "Cabal" ps); };
        })
        ({ pkgs, lib, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isGhcjs {
          packages.ormolu = {
            flags.fixity-th = false;
            writeHieFiles = lib.mkForce false;
          };
          packages.ormolu-live.ghcOptions =
            lib.optional (!ormoluLiveLink) "-fno-code";
        })
        (gitTH "ormolu" "")
        (gitTH "ormolu-live" "../")
      ];
  };
  ormolu = hsPkgs.ormolu;
  ormoluLib = ormolu.components.library;
  ormoluExe = ormolu.components.exes.ormolu;
  ormolize = import ./nix/ormolize {
    inherit pkgs;
    ormolu = ormoluExe;
  };
  extractHackageInfo = hsPkgs.extract-hackage-info.components.exes.extract-hackage-info;
  ormoluLive = hsPkgs.projectCross.ghcjs.hsPkgs.ormolu-live.components.exes.ormolu-live
    .overrideAttrs (_: pkgs.lib.optionalAttrs (!ormoluLiveLink) {
      installPhase = ''
        mkdir -p $out
      '';
    });

  expectedFailures = [
    "Agda"
    "esqueleto"
    "haxl"
    "hlint"
    "idris"
    "leksah"
    "pipes"
    "postgrest"
  ];
  ormolizedPackages =
    let
      ormolizeOverlay = self: super: { };
      ormolizablePackages = pkgs.haskellPackages.override {
        overrides = ormolizeOverlay;
      };

    in doCheck: pkgs.lib.mapAttrs (name: p: ormolize {
        package = p;
        inherit doCheck;
        expectedFailures =
          if pkgs.lib.lists.any (x: x == name) expectedFailures
            then ./expected-failures + "/${name}.txt"
            else null;
      }) ormolizablePackages;
in {
  ormoluTests = ormolu.checks.tests;
  ormolu = ormoluExe; # for compatibility
  inherit ormoluLib ormoluExe ormoluCompiler;
  dev = let shellFor = packages: hsPkgs.shellFor {
    inherit packages;
    tools = {
      cabal = "latest";
      haskell-language-server = "latest";
    };
    withHoogle = false;
    exactDeps = false;
  }; in {
    inherit hsPkgs;
    ormoluShell = shellFor (ps: [ ps.ormolu ]);
    ormoluLiveShell = shellFor (ps: [ ps.ormolu-live ]);
    extractHackageInfoShell = shellFor (ps: [ ps.extract-hackage-info ]);
    cabalAndOrmolu = pkgs.mkShell {
      buildInputs = [
        (hsPkgs.tool "cabal" "latest")
        ormoluExe
      ];
    };
  };
  hackage = ormolizedPackages false;
  hackageTests = with pkgs.lib; pkgs.recurseIntoAttrs (
    let ps = [
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
      "text_2_0"
      "tls"
      "unpacked-containers"
      "yesod-core"
    ];
    in listToAttrs (map (p: nameValuePair p (ormolizedPackages true).${p}) ps)
  );
  regionTests = pkgs.stdenv.mkDerivation {
    name = "ormolu-region-tests";
    src = ./region-tests;
    buildInputs = [
      ormoluExe
      pkgs.diffutils
    ];
    doCheck = true;
    buildPhase = ''
      cp src.hs result-all-implicit.hs
      ormolu --check-idempotence --mode inplace result-all-implicit.hs
      cp src.hs result-all-explicit.hs
      ormolu --check-idempotence --mode inplace --start-line 1 --end-line 18 result-all-explicit.hs
      cp src.hs result-only-start.hs
      ormolu --check-idempotence --mode inplace --start-line 1 result-only-start.hs
      cp src.hs result-only-end.hs
      ormolu --check-idempotence --mode inplace --end-line 18 result-only-end.hs
      cp src.hs result-6-7.hs
      ormolu --check-idempotence --mode inplace --start-line 6 --end-line 7 result-6-7.hs
      cp src.hs result-6-8.hs
      ormolu --check-idempotence --mode inplace --start-line 6 --end-line 8 result-6-8.hs
      cp src.hs result-9-12.hs
      ormolu --check-idempotence --mode inplace --start-line 9 --end-line 12 result-9-12.hs
      cp src.hs result-17-18.hs
      ormolu --check-idempotence --mode inplace --start-line 17 --end-line 18 result-17-18.hs
    '';
    checkPhase = ''
      echo result-all-implicit.hs
      diff --color=always expected-result-all.hs result-all-implicit.hs
      echo result-all-explicit.hs
      diff --color=always expected-result-all.hs result-all-explicit.hs
      echo result-only-start.hs
      diff --color=always expected-result-all.hs result-only-start.hs
      echo result-only-end.hs
      diff --color=always expected-result-all.hs result-only-end.hs
      echo result-6-7.hs
      diff --color=always expected-result-6-7.hs result-6-7.hs
      echo result-6-8.hs
      diff --color=always expected-result-6-8.hs result-6-8.hs
      echo result-9-12.hs
      diff --color=always expected-result-9-12.hs result-9-12.hs
      echo result-17-18.hs
      diff --color=always expected-result-17-18.hs result-17-18.hs
    '';
    installPhase = ''
      mkdir "$out"
      find . -name '*.hs' -exec cp --parents {} $out \;
    '';
  };
  binaries = {
    Linux = hsPkgs.projectCross.musl64.hsPkgs.ormolu.components.exes.ormolu;
    macOS = pkgs.runCommand "ormolu-macOS" {
      buildInputs = [ pkgs.macdylibbundler ];
    } ''
      mkdir -p $out/bin
      cp ${ormoluExe}/bin/ormolu $out/bin/ormolu
      chmod 755 $out/bin/ormolu
      dylibbundler -b \
        -x $out/bin/ormolu \
        -d $out/bin \
        -p '@executable_path'
    '';
    Windows = hsPkgs.projectCross.mingwW64.hsPkgs.ormolu.components.exes.ormolu;
  };
} // pkgs.lib.optionalAttrs (pkgs.lib.hasPrefix "ghc810" ormoluCompiler) {
  inherit extractHackageInfo;
  weeder = pkgs.runCommand
    "ormolu-weeder" {
      buildInputs = [
        ormoluExe
        # Weeder >= 2.3 requires an ugly workaround:
        # https://github.com/ocharles/weeder/pull/81
        (hsPkgs.tool "weeder" "2.2.0")
      ];
    } ''
      mkdir -p $out
      export XDG_CACHE_HOME=$TMPDIR/cache
      weeder --config ${./weeder.dhall} \
        --hie-directory ${ormoluLib.hie} \
        --hie-directory ${ormoluExe.hie} \
        --hie-directory ${ormolu.components.tests.tests.hie}
    '';
  ormoluLive = {
    inherit ormoluLive;
    website = pkgs.stdenv.mkDerivation {
      name = "ormolu-live-website";
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "ormolu-live-www";
        src = ./.;
        subDir = "ormolu-live/www";
      };
      buildInputs = [ pkgs.closurecompiler ];
      installPhase = ''
        mkdir -p $out
        find . \( -name '*.html' -o -name '*.css' \) -exec cp {} $out \;
        ORMOLU_LIVE=${ormoluLive}/bin/ormolu-live.jsexe
        closure-compiler \
          $ORMOLU_LIVE/all.js --externs $ORMOLU_LIVE/all.js.externs \
          -O ADVANCED --jscomp_off=checkVars -W QUIET \
          --js_output_file $out/all.min.js
      '';
    };
  };
}
