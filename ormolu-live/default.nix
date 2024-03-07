{ pkgs, inputs }:

let
  inherit (pkgs) system;
in
{
  shell = pkgs.mkShell {
    packages = [
      inputs.ghc-wasm-meta.packages.${system}.all_gmp
      pkgs.esbuild
      pkgs.npm-check-updates
    ];
  };
}
