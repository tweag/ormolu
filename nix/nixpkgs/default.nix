let
  rev = "454eea84a757ca5f733c4ec0f234eba2281c74eb";
  sha256 = "1k9jbix4w43brqlfmvwy218pf5fbmzsnc08shaww9qfdl1rdlaxy";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs { config.allowUnfree = true; };
in pkgs

