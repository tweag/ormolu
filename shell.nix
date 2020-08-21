{ pkgs ? (import ./nix/nixpkgs { inherit system; })
, system ? builtins.currentSystem
}:

(import ./default.nix { inherit pkgs; }).dev.ormoluShell
