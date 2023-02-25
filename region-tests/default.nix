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
      ormolu --check-idempotence --mode inplace --start-line 1 --end-line 23 result-all-explicit.hs
      cp src.hs result-only-start.hs
      ormolu --check-idempotence --mode inplace --start-line 1 result-only-start.hs
      cp src.hs result-only-end.hs
      ormolu --check-idempotence --mode inplace --end-line 23 result-only-end.hs
      cp src.hs result-8-9.hs
      ormolu --check-idempotence --mode inplace --start-line 8 --end-line 9 result-8-9.hs
      cp src.hs result-8-10.hs
      ormolu --check-idempotence --mode inplace --start-line 8 --end-line 10 result-8-10.hs
      cp src.hs result-11-14.hs
      ormolu --check-idempotence --mode inplace --start-line 11 --end-line 14 result-11-14.hs
      cp src.hs result-19-23.hs
      ormolu --check-idempotence --mode inplace --start-line 19 --end-line 23 result-19-23.hs
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
      echo result-8-9.hs
      diff --color=always expected-result-8-9.hs result-8-9.hs
      echo result-8-10.hs
      diff --color=always expected-result-8-10.hs result-8-10.hs
      echo result-11-14.hs
      diff --color=always expected-result-11-14.hs result-11-14.hs
      echo result-19-23.hs
      diff --color=always expected-result-19-23.hs result-19-23.hs
    '';
    installPhase = ''
      mkdir "$out"
      find . -name '*.hs' -exec cp --parents {} $out \;
    '';
  };
}
