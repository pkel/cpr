{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.linkchecker
    pkgs.nodejs
    pkgs.nodePackages.npm

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
