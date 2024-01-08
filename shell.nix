{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    buildInputs = [
        pkgs.awscli2
        pkgs.jq
    ];
}
