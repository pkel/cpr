let pinned =
  import (fetchTarball https://github.com/nixos/nixpkgs/archive/56cbe42f1668338d05febfbb866e32f2c865609a.tar.gz) {};
in

{ pkgs ? pinned }:

pkgs.mkShell {
  buildInputs = [
    pkgs.R
    pkgs.rPackages.igraph

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
