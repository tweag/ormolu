{ system ? builtins.currentSystem }:
let
  rev = "84d74ae9c9cbed73274b8e4e00be14688ffc93fe";
  sha256 = "0ww70kl08rpcsxb9xdx8m48vz41dpss4hh3vvsmswll35l158x0v";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
in pkgs
