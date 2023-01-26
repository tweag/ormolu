{ pkgs, inputs, defaultGHC }:

let
  inherit (pkgs) system lib;
  npmlock2nix = (pkgs.callPackage inputs.npmlock2nix { }).v2;
  ps-tools = inputs.ps-tools.legacyPackages.${system}.for-0_15;
  purs-nix = inputs.purs-nix { inherit system; };
  ps = purs-nix.purs {
    dependencies = [ "halogen" "ace" "profunctor-lenses" ];
    dir = ./.;
  };
  es-opt = npmlock2nix.build {
    src = ./.;
    installPhase = "cp -r output-es $out";
    buildCommands = lib.singleton ''
      purs-backend-es build --int-tags \
        --corefn-dir ${ps.output { codegen = "corefn"; }}
    '';
  };
  metadata = builtins.toJSON {
    inherit (inputs.self.packages.${system}.default) version;
    inherit (inputs.self) rev;
    ghcAPIVersion =
      defaultGHC.dev.hsPkgs.ghc-lib-parser.components.library.version;
  };
  ghcWasmDeps = [ inputs.ghc-wasm-meta.packages.${system}.default ];
in
{
  package = npmlock2nix.build {
    src = ./.;
    installPhase = "cp -r dist $out";
    buildCommands = lib.optional (inputs.self ? rev) ''
      echo ${lib.escapeShellArg metadata} > src/meta.json
    '' ++ lib.singleton ''
      cp -r ${es-opt} output
      date > src/ormolu.wasm
      cp --remove-destination \
        ${../extract-hackage-info/hackage-info.bin} src/hackage-info.bin
      parcel build --no-source-maps www/index.html
    '';
  };
  shell = npmlock2nix.shell {
    src = ./.;
    buildInputs = [
      pkgs.nodejs
      pkgs.watchexec
      (ps.command { })
      ps-tools.purs-tidy
      ps-tools.purescript
    ] ++ ghcWasmDeps;
  };
  ghcWasmShell = pkgs.mkShell { packages = [ ghcWasmDeps ]; };
  inherit (ps-tools) purs-tidy;
}
