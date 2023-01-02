{ pkgs, ormolu }:

{
  fixityTests = pkgs.stdenv.mkDerivation {
    name = "ormolu-fixity-tests";
    src = ./.;
    buildInputs = [
      ormolu
      pkgs.diffutils
    ];
    doCheck = true;
    buildPhase = ''
      cp test-0-input.hs test-0-no-extra-info.hs
      ormolu --check-idempotence --mode inplace --no-cabal test-0-no-extra-info.hs
      cp test-0-input.hs test-0-with-fixity-info-manual.hs
      ormolu --check-idempotence --mode inplace --no-cabal --fixity 'infixr 8 .=' test-0-with-fixity-info-manual.hs
      cp test-0-input.hs test-0-with-fixity-info-dotormolu.hs
      ormolu --check-idempotence --mode inplace test-0-with-fixity-info-dotormolu.hs
      cp test-1-input.hs test-1-no-extra-info.hs
      ormolu --check-idempotence --mode inplace --no-cabal test-1-no-extra-info.hs
      cp test-1-input.hs test-1-with-fixity-info-manual.hs
      ormolu --check-idempotence --mode inplace --no-cabal --fixity 'infixr 8 .=' --fixity 'infixr 5 #' test-1-with-fixity-info-manual.hs
      cp test-1-input.hs test-1-with-fixity-info-dotormolu.hs
      ormolu --check-idempotence --mode inplace test-1-with-fixity-info-dotormolu.hs
    '';
    checkPhase = ''
      echo test-0-no-extra-info.hs
      diff --color=always test-0-no-extra-info-expected.hs test-0-no-extra-info.hs
      echo test-0-with-fixity-info-manual.hs
      diff --color=always test-0-with-fixity-info-expected.hs test-0-with-fixity-info-manual.hs
      echo test-0-with-fixity-info-dotormolu.hs
      diff --color=always test-0-with-fixity-info-expected.hs test-0-with-fixity-info-dotormolu.hs
      echo test-1-no-extra-info.hs
      diff --color=always test-1-no-extra-info-expected.hs test-1-no-extra-info.hs
      echo test-1-with-fixity-info-manual.hs
      diff --color=always test-1-with-fixity-info-expected.hs test-1-with-fixity-info-manual.hs
      echo test-1-with-fixity-info-dotormolu.hs
      diff --color=always test-1-with-fixity-info-expected.hs test-1-with-fixity-info-dotormolu.hs
    '';
    installPhase = ''
      mkdir "$out"
      find . -name '*.hs' -exec cp --parents {} $out \;
    '';
  };
}
