let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
  inherit (haskellNix) nixpkgsArgs;
  overlays = nixpkgsArgs.overlays ++ [
    (self: super: {
      closurecompiler = super.closurecompiler.overrideAttrs (old: rec {
        version = "20211107";
        src = super.fetchurl {
          url = "https://repo1.maven.org/maven2/com/google/javascript/closure-compiler/v${version}/closure-compiler-v${version}.jar";
          sha256 = "733f00f0a1651c9d5409d9162e6f94f0a3e61463628925d3d6ef66be60ec14a6";
        };
      });
    })
  ];
in
import haskellNix.sources.nixpkgs-unstable (nixpkgsArgs // { inherit overlays; })
