{ system, inputs }:

let
  pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
in
{
  shell = pkgs.mkShell {
    packages = [
      inputs.ghc-wasm-meta.packages.${system}.all_9_10
      pkgs.esbuild
      pkgs.npm-check-updates
      pkgs.miniserve
    ];
  };
}
