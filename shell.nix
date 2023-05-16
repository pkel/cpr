{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    autoconf
    gcc
    gnumake
    opam
    parallel
    pkg-config
    python39
    zip
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib:$LD_LIBRARY_PATH"
  '';
}
