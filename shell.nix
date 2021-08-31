{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    bash
    cacert
    coreutils
    gnugrep
    jo
    jq
    mktemp
    pandoc
    parallel
    pup
    wget
  ];
}
