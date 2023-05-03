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
      ormolu --check-idempotence --mode inplace --no-cabal --fixity 'infixr 8 .=' --fixity 'infixr 5 :>' test-0-with-fixity-info-manual.hs
      cp test-0-input.hs test-0-with-fixity-info-dotormolu.hs
      ormolu --check-idempotence --mode inplace -p base test-0-with-fixity-info-dotormolu.hs
      cp test-1-input.hs test-1-no-extra-info.hs
      ormolu --check-idempotence --mode inplace --no-cabal test-1-no-extra-info.hs
      cp test-1-input.hs test-1-with-fixity-info-manual.hs
      ormolu --check-idempotence --mode inplace --no-cabal --fixity 'infixr 8 .=' --fixity 'infixr 5 #' test-1-with-fixity-info-manual.hs
      cp test-1-input.hs test-1-with-fixity-info-dotormolu.hs
      ormolu --check-idempotence --mode inplace -p base test-1-with-fixity-info-dotormolu.hs
      cp test-1-input.hs test-1-with-fixity-info-weird-overwrite.hs
      ormolu --check-idempotence --mode inplace -p base --fixity "infixr 5 $" test-1-with-fixity-info-weird-overwrite.hs
      cp test-2-input.hs test-2-no-extra-info.hs
      ormolu --check-idempotence --mode inplace --no-cabal -p base -p lens test-2-no-extra-info.hs
      cp test-2-input.hs test-2-reexports-manual.hs
      ormolu --check-idempotence --mode inplace --no-cabal -p base -p lens --reexport 'module Foo exports Control.Lens' test-2-reexports-manual.hs
      cp test-2-input.hs test-2-reexports-dotormolu.hs
      ormolu --check-idempotence --mode inplace -p base -p lens test-2-reexports-dotormolu.hs
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
      echo test-1-with-fixity-info-weird-overwrite.hs
      diff --color=always test-1-with-fixity-info-weird-overwrite-expected.hs test-1-with-fixity-info-weird-overwrite.hs
      echo test-2-no-extra-info.hs
      diff --color=always test-2-no-extra-info.hs test-2-no-extra-info-expected.hs
      echo test-2-reexports-manual.hs
      diff --color=always test-2-reexports-manual.hs test-2-with-reexports-expected.hs
      echo test-2-reexports-dotormolu.hs
      diff --color=always test-2-reexports-dotormolu.hs test-2-with-reexports-expected.hs
    '';
    installPhase = ''
      mkdir "$out"
      find . -name '*.hs' -exec cp --parents {} $out \;
    '';
  };
}
