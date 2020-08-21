{ system ? builtins.currentSystem }:
let
  rev = "807ca93fadd5197c2260490de0c76e500562dc05";
  sha256 = "10yq8bnls77fh3pk5chkkb1sv5lbdgyk1rr2v9xn71rr1k2x563p";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
in pkgs
