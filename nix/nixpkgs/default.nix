{ system ? builtins.currentSystem }:
let
  rev = "32b46dd897ab2143a609988a04d87452f0bbef59";
  sha256 = "1gzfrpjnr1bz9zljsyg3a4zrhk8r927sz761mrgcg56dwinkhpjk";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
in pkgs
