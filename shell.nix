{ pkgs ? import <nixpkgs> { } }:

let
  hsPkgs = pkgs.haskellPackages.ghcWithPackages (p:
    with p; [
      wreq
      lens
      rio
      taggy-lens
      haskell-language-server
      retry
      tasty
      tasty-golden
      tasty-hedgehog
      tasty-hunit
      tasty-hspec
      temporary
      token-bucket
    ]);

in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    hsPkgs

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
