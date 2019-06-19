{ lib }:

let
  rev = "ec5dd0536a5e4c3a99c797b86180f7261197c124";
  sha256 = "0k2r8y21rn4kr5dmddd3906x0733fs3bb8hzfpabkdav3wcy3klv";
  url = "https://github.com/hercules-ci/gitignore/archive/${rev}.tar.gz";
  nixGitIgnore = builtins.fetchTarball { inherit url sha256; };
in (import nixGitIgnore { inherit lib; }).gitignoreSource
