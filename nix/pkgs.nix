let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
  inherit (haskellNix) nixpkgsArgs;
  overlays = nixpkgsArgs.overlays ++ [
    (self: super: {
      macdylibbundler = super.macdylibbundler.overrideAttrs (old: {
        version = "custom";
        src = super.fetchFromGitHub {
          owner = "amesgen";
          repo = "macdylibbundler";
          rev = "f98d06980e718947b1d4afed3586f475056563b6";
          sha256 = "0qqgcp14vvr6r5yqj96hy3klc2dkcqyq909az30ql4y879r3xm62";
        };
      });
    })
  ];
in
import haskellNix.sources.nixpkgs-unstable (nixpkgsArgs // { inherit overlays; })
