let pinned =
  import (fetchTarball https://github.com/nixos/nixpkgs/archive/56cbe42f1668338d05febfbb866e32f2c865609a.tar.gz) {};
in

{ pkgs ? pinned }:

pkgs.mkShell {
  buildInputs = [
    pkgs.evince
    pkgs.texlive.combined.scheme-full

    (pkgs.callPackage ../../../tools/textidote.nix {})

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
