{ pkgs, haskellPackages }: _: p:
  pkgs.stdenv.mkDerivation {
    name = p.name + "-ormolized";
    src = p.src;
    buildInputs = [
      haskellPackages.cpphs
      haskellPackages.ormolu
      pkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    buildPhase = ''
      find . -name '*.hs' -exec bash ${./ormolize.sh} {} \; 2> log.txt
    '';
    installPhase = ''
      mkdir "$out"
      find . -name '*.hs' -exec cp --parents {} $out \;
      cp log.txt $out/log.txt
    '';
  }
