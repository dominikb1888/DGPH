{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    pkgs.semeru-bin
    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
