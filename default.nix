{ ormoluCompiler ? "ghc8107"
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
    modules = [({pkgs, ...}: {
      dontStrip = false;
      dontPatchELF = false;
      enableDeadCodeElimination = true;
      packages.ormolu.components.exes.ormolu.build-tools =
        pkgs.lib.mkForce [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];
      packages.ormolu.components.exes.ormolu.extraSrcFiles = [ ".git/**/*" ];
    })];
  };
  ormolu = hsPkgs.ormolu;
  ormoluExe = ormolu.components.exes.ormolu;
  ormolize = import ./nix/ormolize {
    inherit pkgs;
    ormolu = ormoluExe;
  };

  expectedFailures = [
    "Agda"
    "esqueleto"
    "graphql-engine"
    "haxl"
    "hlint"
    "idris"
    "intero"
    "leksah"
    "pandoc"
    "pipes"
    "postgrest"
  ];
  ormolizedPackages =
    let
      ormolizeOverlay = self: super: {
        "graphql-engine" = {
          name = "graphql-engine";
          src = pkgs.fetchFromGitHub {
            owner = "hasura";
            repo = "graphql-engine";
            rev = "v2.0.5";
            sha256 = "0sw7jwj3ixr0nnh3i9yagiqjsvf83w79jd7ac9vdvm410jfjcnxf";
          };
        };
      };
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
  ormoluLib = ormolu.components.library;
  ormoluTests = ormolu.checks.tests;
  ormolu = ormoluExe; # for compatibility
  inherit ormoluExe ormoluCompiler;
  dev = {
    ormoluShell = hsPkgs.shellFor {
      tools = { cabal = "latest"; };
      withHoogle = false;
      exactDeps = true;
    };
    withOrmolu = hsPkgs.shellFor {
      tools = { cabal = "latest"; };
      withHoogle = false;
      exactDeps = true;
      buildInputs = [ormoluExe];
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
      "servant"
      "servant-server"
      "stack"
      "tensorflow"
      "text_1_2_4_1"
      "tls"
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
}
