{ pkgs
, ormolu
}:
{ package
, expectedFailures ? null
, doCheck ? false
}:
  pkgs.stdenv.mkDerivation rec {
    name = package.name + "-ormolized";
    src = package.src;
    buildInputs = [
      ormolu
      pkgs.haskellPackages.cpphs
      pkgs.diffutils
      pkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    buildPhase = ''
      hs_files=$(find . -name '*.hs' -o -name '*.hsig')
      for hs_file in $hs_files; do

        # drop includes
        sed -i '/^#include/d' "$hs_file"

        # deal with CPP
        cpphs "$hs_file" --noline -DARCH_X86 > "''${hs_file}-nocpp" 2> /dev/null

        # annoyingly, cpphs cannot modify files in place
        mv "''${hs_file}-nocpp" "$hs_file"

        # preserve the original
        cp "$hs_file" "''${hs_file}-original"
      done

      ((ormolu --check-idempotence --mode inplace $hs_files; echo $? > exit_code) || true) 2> log.txt
    '';
    inherit doCheck;
    checkPhase =
      if expectedFailures == null
        then ''
          echo "No failures expected"
          if (( $(cat exit_code) != 0 )); then exit 1; fi
        ''
        else ''
          diff --ignore-blank-lines --color=always ${expectedFailures} log.txt
        '';
    installPhase = ''
      mkdir "$out"
      find . \( -name '*.hs-original' -o -name '*.hs' -o -name '*.hsig-original' -o -name '*.hsig' -o -name '*.cabal' \) -exec cp --parents {} $out \;
      cp log.txt $out/log.txt
    '';
  }
