{ system ? builtins.currentSystem }:
let
  rev = "16105403bdd843540cbef9c63fc0f16c1c6eaa70";
  sha256 = "0sl6hsxlh14kcs38jcra908nvi5hd8p8hlim3lbra55lz0kd9rcl";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
in pkgs
