{ system, inputs }:

let
  pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
  inherit (pkgs) lib;
in
{
  shell = pkgs.mkShell {
    packages = [
      inputs.ghc-wasm-meta.packages.${system}.all_9_10
      pkgs.esbuild
      pkgs.npm-check-updates
      pkgs.miniserve
    ];

    # Otherwise there are `happy` errors in GHA CI.
    shellHook = ''
      export LANG="en_US.UTF-8"
    '' + lib.optionalString
      (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
    '';
  };
}
