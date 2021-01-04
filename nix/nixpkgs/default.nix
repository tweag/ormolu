{ system ? builtins.currentSystem }:
let
  rev = "56bb1b0f7a33e5d487dc2bf2e846794f4dcb4d01";
  sha256 = "1wl5yglgj3ajbf2j4dzgsxmgz7iqydfs514w73fs9a6x253wzjbs";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
in pkgs
