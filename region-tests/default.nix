{ pkgs, ormolu }:

{
  regionTests = pkgs.stdenv.mkDerivation {
    name = "ormolu-region-tests";
    src = ./.;
    buildInputs = [
      ormolu
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
}
