{ pkgs, haskellPackages }: { package, doCheck ? false }:
  pkgs.stdenv.mkDerivation {
    name = package.name + "-ormolized";
    src = package.src;
    buildInputs = [
      haskellPackages.cpphs
      (if doCheck
        then haskellPackages.ormolu
        else pkgs.haskell.lib.dontCheck haskellPackages.ormolu)
      pkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    buildPhase = ''
      find . -name '*.hs' -exec bash ${./ormolize.sh} {} \; 2> log.txt
      cat log.txt
    '';
    inherit doCheck;
    checkPhase = ''
       if [[ -s log.txt ]]; then exit 1; fi
    '';
    installPhase = ''
      mkdir "$out"
      find . -name '*.hs-original' -exec cp --parents {} $out \;
      find . -name '*.hs' -exec cp --parents {} $out \;
      cp log.txt $out/log.txt
    '';
  }
