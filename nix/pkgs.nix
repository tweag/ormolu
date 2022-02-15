let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
in
import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
