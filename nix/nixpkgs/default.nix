let
  rev = "3f4144c30a6351dd79b177328ec4dea03e2ce45f";
  sha256 = "1qg5n60n3fr6cypihnrjw451fadps5pysj5p0vvfb320mpfvlbjb";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs { config.allowUnfree = true; };
in pkgs
